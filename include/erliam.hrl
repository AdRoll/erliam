-type aws_datetime() :: string(). % "YYYYMMDDTHHMMSSZ"
-type iso_datetime() :: string(). % "YYYY-MM-DDTHH:MM:SSZ"

-record(credentials, {
          expiration        :: undefined | iso_datetime(),
          security_token    :: undefined | string(), % required when using temporary credentials
          secret_access_key :: string(),
          access_key_id     :: string()
         }).
