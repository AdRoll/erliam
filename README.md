# erliam

erlang library for caching credentials and signing AWS API requests.

## usage

1. Add `erliam` to application dependencies or do `application:start(erliam)`.

2. Call `erliam:credentials()` to obtain latest cached credentials (stored in ets and
automatically refreshed before expiry).

3. Call `awsv4:headers(Credentials, Parameters)` to obtain awsv4-signed request headers to
use in AWS API calls.

If not using instance metadata, set `aws_access_key` and `aws_secret_key` in `erliam`
application environment to your long-term credentials; these will be used to obtain a
session token periodically.

## IMDSv2 Support

This library supports **AWS Instance Metadata Service Version 2 (IMDSv2)** by default, which
provides enhanced security against SSRF attacks. IMDSv2 uses session-oriented requests with
a token that must be obtained before accessing metadata.

### Configuration

- `imds_use_v2` (default: `true`) - Enable IMDSv2 support. If token retrieval fails, the
  library will automatically fall back to IMDSv1.
- `imds_token_ttl` (default: `21600` seconds / 6 hours) - The TTL for IMDSv2 session tokens.
- `imds_host` (default: `"169.254.169.254"`) - The IMDS host address.
- `imds_version` (default: `"latest"`) - The IMDS API version.

## example

### Fetch an object from S3

```
> application:start(erliam).
> QueryParams = #{"prefix" => "some/prefix/",
                  "delimiter" => "/",
                  "list-type" => "2",
                  "encoding-type" => "url"}.
> Headers = awsv4:headers(erliam:credentials(),
                          #{service => "s3",
                            region => "us-west-2",
                            host => "bucketname.s3.amazonaws.com",
                            path => "/",
                            query_params => QueryParams}).
> httpc:request(get, {lists:flatten("https://bucketname.s3.amazonaws.com/?" ++
                                    awsv4:canonical_query(QueryParams)), Headers}, [], []).

{ok, {{"HTTP/1.1", 200, "OK"},
 [{"date", "Fri, 02 Jun 2017 23:26:21 GMT"},
  {"server", "AmazonS3"},
  {"content-length", "496"},
  {"content-type", "application/xml"},
  {"x-amz-id-2", "SOME-ID"},
  {"x-amz-request-id", "SOME-OTHER-ID"},
  {"x-amz-bucket-region", "us-west-2"}],
 "<?xml version=\"1.0\" encoding=\"UTF-8\"?>\n<ListBucketResult xmlns=\"http://s3.amazonaws.com/doc/2006-03-01/\"><Name>bucketname</Name>..."}}
```

### Encrypt plaintext using KMS

```
> application:start(erliam).
> QueryParams = #{}.
> KeyId = <<"xxxx-xxxx-xxxx-xxxx-xxxxxxxxxx">>
> RequestBody = jiffy:encode(#{<<"EncryptionContext">> => #{<<"application">> => <<"thing encryptor">>},
                               <<"KeyId">> => KeyId,
                               <<"Plaintext">> => base64:encode(<<"setec astronomy">>)}).
> SignedHeaders = #{"content-type" => "application/x-amz-json-1.1"}.
> Headers = awsv4:headers(erliam:credentials(),
                          #{service => "kms",
                            target_api => "TrentService.Encrypt",
                            method => "POST",
                            region => "us-east-1",
                            query_params => QueryParams,
                            signed_headers => SignedHeaders},
                          RequestBody).
> httpc:request(post, {lists:flatten(["https://kms.us-east-1.amazonaws.com", "/?",
                                      awsv4:canonical_query(QueryParams)]),
                       Headers,
                       proplists:get_value("content-type", Headers), RequestBody}, [], []).

{ok, {{"HTTP/1.1", 200, "OK"},
     [{"content-length", "307"},
      {"content-type", "application/x-amz-json-1.1"},
      {"x-amzn-requestid", "xxxxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxx"}],
     "{\"CiphertextBlob\":\"B64ENCODED CIPHERTEXT\",\"KeyId\":\"KEY ARN\"}"}}
```
