# Unfeature: Expand Known Rust Features

Removes all blocks that are disabled by features and the attributes of enabled features.

This tool can be used to create handouts for programming assignments.

## Usage

This is a cargo tool, that can handle entire crates (including local workspace dependencies).

```
cargo unfeature <output_dir> <match_features> [additional_files...] [options]
Options:
  -F, --features <FEATURES>            Enabled features
      --no-default-features            Disable default features
      --all-features                   Enable all features
      --manifest-path <MANIFEST_PATH>  Path to Cargo.toml
```

This tool creates a copy of the crate in `<output_dir>` with the specified features being expanded.

It also removes disabled optional dependencies from the `Cargo.toml` manifest.
