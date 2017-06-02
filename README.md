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

## example

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
                            path => "/"
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
