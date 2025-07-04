use super::*;
use biome_fs::BiomePath;
use biome_rowan::NodeCache;
use std::path::Path;

#[test]
fn test_svelte_format() {
    // Create a simple Svelte file with HTML, JavaScript, and CSS
    let svelte_content = r#"<script>
  const count = 0;
  function increment() {
    count = count + 1;
  }
</script>

<style>
  button {
    background-color: #ff3e00;
    color: white;
    border: none;
    padding: 8px 12px;
    border-radius: 4px;
  }
</style>

<main>
  <h1>Count: {count}</h1>
  <button on:click={increment}>Increment</button>
</main>
"#;

    // Parse the Svelte file
    let mut cache = NodeCache::default();
    let biome_path = BiomePath::new(Path::new("test.svelte")).unwrap();
    let file_source = DocumentFileSource::String(svelte_content.to_string());
    let settings = Settings::default();

    let parse_result = parse(&biome_path, file_source.clone(), svelte_content, &settings, &mut cache);
    
    // Format the Svelte file
    let format_result = format(&biome_path, &file_source, parse_result.any_parse, &settings);
    
    // Verify the formatting result
    assert!(format_result.is_ok(), "Formatting should succeed");
    
    let formatted = format_result.unwrap().into_code();
    
    // Check that the script tag is properly formatted
    assert!(formatted.contains("const count = 0;"), "Script content should be preserved");
    
    // Check that the style tag is properly formatted
    assert!(formatted.contains("background-color: #ff3e00;"), "Style content should be preserved");
    
    // Check that the HTML is properly formatted
    assert!(formatted.contains("<button on:click={increment}>"), "HTML content should be preserved");
}
