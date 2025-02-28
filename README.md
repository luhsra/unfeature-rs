# Unfeature: Expand Known Rust Features

Removes all blocks that are disabled by features and the attributes of enabled features.

## Usage

This is a clang tool, that can handle entire crates (and local workspace dependencies).

```
cargo unfeature <destination_dir> <mach_features> <...additional_files> [<args>]
Options:
  -F, --features <FEATURES>            Enabled features
      --no-default-features            Disable default features
      --all-features                   Enable all features
      --manifest-path <MANIFEST_PATH>  Path to Cargo.toml
```

This tool crates a copy of the crate in `<destination_dir>` with the specified features being inlined features.

It also removes disabled optional dependencies.
