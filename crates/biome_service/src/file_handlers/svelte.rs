use crate::WorkspaceError;
use crate::file_handlers::{
    AnalyzerCapabilities, Capabilities, CodeActionsParams, DebugCapabilities, EnabledForPath,
    ExtensionHandler, FixAllParams, FormatterCapabilities, LintParams, LintResults, ParseResult,
    ParserCapabilities, javascript,
};
use crate::settings::Settings;
use crate::workspace::{DocumentFileSource, FixFileResult, PullActionsResult, EmbeddedJsContent, EmbeddedCssContent};
use biome_formatter::Printed;
use biome_fs::BiomePath;
use biome_js_parser::{JsParserOptions, parse_js_with_cache, parse_js_with_offset_and_cache};
use biome_js_syntax::{EmbeddingKind, JsFileSource, TextRange, TextSize};
use biome_parser::AnyParse;
use biome_rowan::{NodeCache, AstNode, AstNodeList};
use biome_css_parser::{CssParserOptions, parse_css_with_offset_and_cache};
use biome_css_formatter;
use biome_css_syntax;
use biome_html_formatter::{HtmlFormatOptions, format_node, context::IndentScriptAndStyle};
use biome_html_parser::parse_html_with_cache;
use biome_html_syntax::HtmlFileSource;
use regex::{Match, Regex};
use std::sync::LazyLock;
use tracing::debug;

use super::{SearchCapabilities, parse_lang_from_script_opening_tag};

#[derive(Debug, Default, PartialEq, Eq)]
pub struct SvelteFileHandler;

// https://regex101.com/r/E4n4hh/6
pub static SVELTE_SCRIPT_FENCE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"(?ixs)(?<opening><script(?:\s.*?)?>)\r?\n?(?<content>(?U:.*?))</script>"#).unwrap()
});

// https://regex101.com/r/E4n4hh/7
pub static SVELTE_STYLE_FENCE: LazyLock<Regex> = LazyLock::new(|| {
    Regex::new(r#"(?ixs)(?<opening><style(?:\s.*?)?>)\r?\n?(?<content>(?U:.*?))</style>"#).unwrap()
});

impl SvelteFileHandler {
    /// It extracts the JavaScript/TypeScript code contained in the script block of a Svelte file
    ///
    /// If there's no script block, an empty string is returned.
    pub fn input(text: &str) -> &str {
        match Self::matches_script(text) {
            Some(script) => &text[script.start()..script.end()],
            _ => "",
        }
    }

    /// It takes the original content of a Svelte file, and new output of an Svelte file. The output is only the content contained inside the
    /// Svelte `<script>` tag. The function replaces `output` inside that `<script>`.
    pub fn output(input: &str, output: &str) -> String {
        if let Some(script) = Self::matches_script(input) {
            format!(
                "{}{}{}",
                &input[..script.start()],
                output,
                &input[script.end()..]
            )
        } else {
            input.to_string()
        }
    }

    /// Returns the start byte offset of the Svelte `<script>` tag
    pub fn start(input: &str) -> Option<u32> {
        Self::matches_script(input).map(|m| m.start() as u32)
    }

    /// Find the content of the script tag in the formatted HTML
    fn matches_script(text: &str) -> Option<Match> {
        SVELTE_SCRIPT_FENCE.captures(text).and_then(|captures| {
            captures.name("content").map(|content| {
                Match::new(content.start(), content.end())
            })
        })
    }

    fn matches_style(input: &str) -> Option<Match> {
        SVELTE_STYLE_FENCE
            .captures(input)
            .and_then(|captures| captures.name("content").map(|content| {
                Match::new(content.start(), content.end())
            }))
    }

    pub fn file_source(text: &str) -> JsFileSource {
        SVELTE_SCRIPT_FENCE
            .captures(text)
            .and_then(|captures| {
                let (language, variant) =
                    parse_lang_from_script_opening_tag(captures.name("opening")?.as_str());
                Some(
                    JsFileSource::from(language)
                        .with_variant(variant)
                        .with_embedding_kind(EmbeddingKind::Svelte),
                )
            })
            .map_or(JsFileSource::js_module(), |fs| fs)
    }
}

impl ExtensionHandler for SvelteFileHandler {
    fn capabilities(&self) -> Capabilities {
        Capabilities {
            enabled_for_path: EnabledForPath {
                formatter: Some(javascript::formatter_enabled),
                search: Some(javascript::search_enabled),
                assist: Some(javascript::assist_enabled),
                linter: Some(javascript::linter_enabled),
            },
            parser: ParserCapabilities { parse: Some(parse) },
            debug: DebugCapabilities {
                debug_syntax_tree: None,
                debug_control_flow: None,
                debug_formatter_ir: None,
                debug_type_info: None,
                debug_registered_types: None,
                debug_semantic_model: None,
            },
            analyzer: AnalyzerCapabilities {
                lint: Some(lint),
                code_actions: Some(code_actions),
                rename: None,
                fix_all: Some(fix_all),
            },
            formatter: FormatterCapabilities {
                format: Some(format),
                format_range: Some(format_range),
                format_on_type: Some(format_on_type),
            },
            // TODO: We should be able to search JS portions already
            search: SearchCapabilities { search: None },
        }
    }
}

fn parse(
    _rome_path: &BiomePath,
    _file_source: DocumentFileSource,
    text: &str,
    _settings: &Settings,
    cache: &mut NodeCache,
) -> ParseResult {
    // Parse the Svelte file as HTML
    let html_parse = parse_html_with_cache(text, HtmlFileSource::svelte(), cache);
    
    // Also parse the script content for JavaScript/TypeScript formatting
    let script = SvelteFileHandler::input(text);
    let file_source = SvelteFileHandler::file_source(text);

    debug!("Parsing Svelte file with script language {:?}", file_source);

    let js_parse = parse_js_with_cache(script, file_source, JsParserOptions::default(), cache);

    // We'll use the JavaScript/TypeScript parse result as the primary one
    // but we'll use the HTML parse result during formatting
    ParseResult {
        any_parse: js_parse.into(),
        language: Some(file_source.into()),
    }
}

/// Extract embedded JavaScript content from Svelte file
fn extract_embedded_scripts(
    text: &str,
    cache: &mut NodeCache,
) -> Vec<EmbeddedJsContent> {
    let mut scripts = Vec::new();
    
    if let Some(captures) = SVELTE_SCRIPT_FENCE.captures(text) {
        if let Some(script_match) = captures.name("script") {
            let script_content = &text[script_match.start()..script_match.end()];
            let script_start = script_match.start() as u32;
            
            let opening = captures.name("opening").map(|m| m.as_str()).unwrap_or("<script>");
            let (language, variant) = parse_lang_from_script_opening_tag(opening);
            
            let file_source = JsFileSource::from(language)
                .with_variant(variant)
                .with_embedding_kind(EmbeddingKind::Svelte);
            
            let parse = parse_js_with_offset_and_cache(
                script_content,
                script_start,
                file_source,
                JsParserOptions::default(),
                cache,
            );
            
            scripts.push(EmbeddedJsContent {
                parse: parse.into(),
                range: TextRange::new(
                    TextSize::from(script_match.start() as u32),
                    TextSize::from(script_match.end() as u32),
                ),
                language: Some(file_source.into()),
            });
        }
    }
    
    scripts
}

/// Extract embedded CSS content from Svelte file
fn extract_embedded_styles(
    text: &str,
    cache: &mut NodeCache,
) -> Vec<EmbeddedCssContent> {
    let mut styles = Vec::new();
    
    if let Some(captures) = SVELTE_STYLE_FENCE.captures(text) {
        if let Some(style_match) = captures.name("style") {
            let style_content = &text[style_match.start()..style_match.end()];
            let style_start = style_match.start() as u32;
            
            let parse = parse_css_with_offset_and_cache(
                style_content,
                style_start,
                CssParserOptions::default(),
                cache,
            );
            
            styles.push(EmbeddedCssContent {
                parse: parse.into(),
                range: TextRange::new(
                    TextSize::from(style_match.start() as u32),
                    TextSize::from(style_match.end() as u32),
                ),
            });
        }
    }
    
    styles
}

#[tracing::instrument(level = "debug", skip(parse, settings))]
fn format(
    biome_path: &BiomePath,
    document_file_source: &DocumentFileSource,
    parse: AnyParse,
    settings: &Settings,
) -> Result<Printed, WorkspaceError> {
    // Get the original text content
    let text = document_file_source.text();
    
    // Parse the file as HTML to format the HTML structure
    let mut cache = NodeCache::default();
    let html_parse = parse_html_with_cache(text, HtmlFileSource::svelte(), &mut cache);
    
    // Get HTML formatting options
    let html_options = HtmlFormatOptions::new(HtmlFileSource::svelte())
        // Always indent script and style tags in Svelte files
        .with_indent_script_and_style(IndentScriptAndStyle::new(true));
    
    // Format the HTML structure
    let html_printed = format_node(
        &html_parse.syntax(),
        &html_options,
        html_parse.comments(),
    )?;
    
    let mut formatted_text = html_printed.into_code();
    
    // Format script content if present
    if let Some(script_match) = SvelteFileHandler::matches_script(&formatted_text) {
        let script_content = &formatted_text[script_match.start()..script_match.end()];
        
        // Get the script tag opening to determine language
        if let Some(captures) = SVELTE_SCRIPT_FENCE.captures(&formatted_text) {
            if let Some(opening) = captures.name("opening") {
                let (language, variant) = parse_lang_from_script_opening_tag(opening.as_str());
                
                // Create a temporary file source with just the script content
                let script_file_source = DocumentFileSource::String(script_content.to_string());
                let file_source = JsFileSource::from(language)
                    .with_variant(variant)
                    .with_embedding_kind(EmbeddingKind::Svelte);
                
                // Parse the script content
                let script_parse = parse_js_with_cache(
                    script_content, 
                    file_source, 
                    JsParserOptions::default(), 
                    &mut cache
                );
                
                // Format the script using the JavaScript formatter
                if let Ok(js_printed) = javascript::format(
                    biome_path,
                    &script_file_source,
                    script_parse.into(),
                    settings,
                ) {
                    // Replace the script content in the formatted HTML
                    formatted_text = format!(
                        "{}{}{}",
                        &formatted_text[..script_match.start()],
                        js_printed.into_code(),
                        &formatted_text[script_match.end()..]
                    );
                }
            }
        }
    }
    
    // Format style content if present
    if let Some(style_match) = SvelteFileHandler::matches_style(&formatted_text) {
        let style_content = &formatted_text[style_match.start()..style_match.end()];
        
        // Create a temporary file source with just the style content
        let style_file_source = DocumentFileSource::String(style_content.to_string());
        
        // Parse the style content
        let style_parse = parse_css_with_offset_and_cache(
            style_content,
            0, // No offset needed here
            CssParserOptions::default(),
            &mut cache,
        );
        
        // Get CSS formatting options and format
        let css_options = settings.format_options::<biome_css_syntax::CssLanguage>(biome_path, &style_file_source);
        
        if let Ok(css_tree) = style_parse.syntax().to_syntax::<biome_css_syntax::CssSyntaxNode>() {
            if let Ok(css_formatted) = biome_css_formatter::format_node(css_options, &css_tree) {
                if let Ok(css_printed) = css_formatted.print() {
                    // Replace the style content in the formatted HTML
                    formatted_text = format!(
                        "{}{}{}",
                        &formatted_text[..style_match.start()],
                        css_printed.into_code(),
                        &formatted_text[style_match.end()..]
                    );
                }
            }
        }
    }
    
    Ok(Printed::from(formatted_text))
}

pub(crate) fn format_range(
    biome_path: &BiomePath,
    document_file_source: &DocumentFileSource,
    parse: AnyParse,
    settings: &Settings,
    range: TextRange,
) -> Result<Printed, WorkspaceError> {
    // For range formatting, we'll delegate to the JavaScript formatter if the range is within a script tag
    // Otherwise, we'll format the entire file
    
    let text = document_file_source.text();
    
    // Check if the range is within a script tag
    if let Some(script_match) = SvelteFileHandler::matches_script(text) {
        let script_range = TextRange::new(
            TextSize::from(script_match.start() as u32),
            TextSize::from(script_match.end() as u32),
        );
        
        if range.start() >= script_range.start() && range.end() <= script_range.end() {
            // The range is within a script tag, delegate to JavaScript formatter
            return javascript::format_range(biome_path, document_file_source, parse, settings, range);
        }
    }
    
    // For other cases, format the entire file
    format(biome_path, document_file_source, parse, settings)
}

pub(crate) fn format_on_type(
    biome_path: &BiomePath,
    document_file_source: &DocumentFileSource,
    parse: AnyParse,
    settings: &Settings,
    offset: TextSize,
) -> Result<Printed, WorkspaceError> {
    // Similar to format_range, delegate to JavaScript formatter if the offset is within a script tag
    
    let text = document_file_source.text();
    
    // Check if the offset is within a script tag
    if let Some(script_match) = SvelteFileHandler::matches_script(text) {
        let script_range = TextRange::new(
            TextSize::from(script_match.start() as u32),
            TextSize::from(script_match.end() as u32),
        );
        
        if offset >= script_range.start() && offset <= script_range.end() {
            // The offset is within a script tag, delegate to JavaScript formatter
            return javascript::format_on_type(biome_path, document_file_source, parse, settings, offset);
        }
    }
    
    // For other cases, format the entire file
    format(biome_path, document_file_source, parse, settings)
}

pub(crate) fn lint(params: LintParams) -> LintResults {
    javascript::lint(params)
}

pub(crate) fn code_actions(params: CodeActionsParams) -> PullActionsResult {
    javascript::code_actions(params)
}

pub(crate) fn fix_all(params: FixAllParams) -> Result<FixFileResult, WorkspaceError> {
    javascript::fix_all(params)
}

#[cfg(test)]
#[path = "svelte.test.rs"]
mod tests;
