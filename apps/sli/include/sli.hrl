%%% ---------------------------------------------------------------
%%% File    : sli.hrl
%%% Author  : Artem Golovinsky artemgolovinsky@gmail.com
%%% Description: 
%%% ---------------------------------------------------------------

%% riakc handler process identifier
-define(RC_HANDLER, rc_handler).

%% riakc supervisor identifier
-define(SV_NAME, sli_riakc_sup).

%% Name of Readers
-define(WC_NAME, "riakc_client").

%% Name of writers

-define(SL_BUCKET, <<"sl">>).

-define(ID_LINK, id_link).

-define(SUP_NAME, sli_sup).

-define(CHARS, "abcdefghjiklmnopqrstuvwxyzABCDEFGHJIKLMNOPQRSTUVWXYZ_-1234567890").

-define(MAX_LENGTH, 6).


-define(MAX_LONG_LINK_LENGTH, 32768).
