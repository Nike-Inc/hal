#!/usr/bin/env bash

UPLOAD_DIR="$HOME/.stack/upload"
CREDS_FILE="$UPLOAD_DIR/credentials.json"

# Setup Travis credentials
mkdir -p "$UPLOAD_DIR"
echo "{ \"username\": \"$HACKAGE_USER\", \"password\": \"$HACKAGE_PASS\" }" > "$CREDS_FILE"

# Upload to Hackage
stack upload .

# Clean up
rm -rf "$CREDS_FILE"
