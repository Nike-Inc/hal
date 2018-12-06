# aws-lambda-runtime

## Local Testing

### Dependencies

- stack
- docker
- aws-sam-cli (>v0.8.0)

### Build

```bash
docker pull fpco/stack-build:lts-12.21 #first build only
stack build --copy-bins
```

### Execute

```bash
echo '{ "value": 7 }' | sam local invoke --region us-east-1
```
