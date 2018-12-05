# aws-lambda-runtime

## Local Testing

### Dependencies

- stack
- docker
- aws-sam-cli (>v0.8.0)

### Build

```bash
stack build
cp .stack-work/install/x86_64-linux-${docker_image_hash}/lts-12.21/8.4.4/bin/aws-lambda-runtime result/bootstrap
```

### Execute

```bash
echo '{ "value": 7 }' | sam local invoke --region us-east-1
```
