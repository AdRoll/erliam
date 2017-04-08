-type aws_datetime() :: string(). % "YYYYMMDDTHHMMSSZ"
-type iso_datetime() :: string(). % "YYYY-MM-DDTHH:MM:SSZ"

-record(credentials, {
          expiration :: iso_datetime(),
          security_token :: string(), % required when using temporary credentials
          secret_access_key :: string(),
          access_key_id :: string()
         }).
