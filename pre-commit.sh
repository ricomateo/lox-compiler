#!/bin/sh
# Run cargo fmt to format the code
echo "Running cargo fmt..."
cargo fmt

# Check if there are any changes after formatting
if ! git diff --quiet; then
    echo "Code was reformatted. Please add the changes and commit again."
    exit 1
fi