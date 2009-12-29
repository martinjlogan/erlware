%%% @doc to make syslog work over udp add the -r option to sysconfig/syslog SYSLOGD_OPTIONS then iperform /etc/init.d/syslog restart
-module(fs_syslog_lib).

-export([
         create_syslog_packet/4, 
	 parse_syslog_packet/1
	]).

%%--------------------------------------------------------------------
%% @doc Create a bsd syslog packet.
%% <pre>
%% Note* The final packet will contain in its MSG portion a Tag field 
%%       consisting of the nodename taken from Node followed by a :space 
%%       and then the Content field. If the nodename to be used for 
%%       the tag field is longer than 32 chars it will be truncated to 32 chars.
%% Variables:
%%  Now - This can be either a now() tuple a datetime() tuple or a pre formatted 
%%        bsd syslog complient date string(see RFC 3164) for details.
%%
%% Types:
%%  Priority = integer()
%%  Now = now() | {date(), time()} | string()
%%  Node = node()
%%  Content = string() | binary()
%% </pre>
%% @spec create_syslog_packet(Priority, Now, Node, Content) -> binary() | exit()
%% @end
%%--------------------------------------------------------------------
create_syslog_packet(Priority, Now, Node, Content) when size(Now) == 3 ->
    create_syslog_packet(Priority, calendar:now_to_local_time(Now), Node, Content);

create_syslog_packet(Priority, DateTime, Node, Content) when size(DateTime) == 2 ->
    create_syslog_packet(Priority, fs_time:date_time_to_string(syslog, DateTime), Node, Content);

create_syslog_packet(Priority, DateTime, Node, RawContent) when is_list(DateTime) ->
    DateBin          = list_to_binary(DateTime),
    [Nodename, Host] = string:tokens(atom_to_list(Node), "@"),

    TruncatedNodename = 
        case length(Nodename) of
    	    Length when Length > 32 -> 
	        lists:sublist(Nodename, 1, 32);
    	    Length -> Nodename
        end,

    case length(StringPri = integer_to_list(Priority)) of
    	Size when Size > 5 -> 
            exit({error, priority});
	Size -> 
            Content = case RawContent of
                          RawContent when is_binary(RawContent) -> RawContent;
                          RawContent when is_list(RawContent)   -> list_to_binary(RawContent)
                      end,
            concat_binary([<<$<>>, StringPri, <<$>>>, DateBin, list_to_binary(Host), $ , list_to_binary(TruncatedNodename), $:, $ ,Content])
    end.


%%--------------------------------------------------------------------
%% @doc Takes a syslog packet and splits it into its componant parts.
%% <pre>
%% Note* see section 5.3 of RFC 3164 for how the MSG segment is separated into Tag and Content.
%%
%% Types:
%%  Priority = integer()
%%  DateTimeString = string()
%%  Host = string()
%%  Tag = Content = string()
%% </pre>
%% @spec parse_syslog_packet(SyslogBin) -> {Priority, DateTimeString, Host, Tag, Content} | exit()
%% @end
%%--------------------------------------------------------------------
parse_syslog_packet(SyslogBin) ->
    {ok, {Pri, RestBin}}       = parse_pri(SyslogBin),
    {ok, {DateTime, RestBin2}} = parse_date_time(RestBin),
    {ok, {Host, MSG}}          = parse_host(RestBin2),
    {ok, {Tag, Content}}       = fs_lists:non_destructive_separate_by_token(MSG, ":[ "), 
    {Pri, DateTime, Host, Tag, Content}.

parse_pri(<<$<, PRI,       $>, RestBin/binary>>) -> {ok, {list_to_integer([PRI]), RestBin}};
parse_pri(<<$<, P,   RI,   $>, RestBin/binary>>) -> {ok, {list_to_integer([P,RI]), RestBin}};
parse_pri(<<$<, P,   R, I, $>, RestBin/binary>>) -> {ok, {list_to_integer([P,R,I]), RestBin}};
parse_pri(_)                                     -> {error, priority}.

parse_date_time(<<DateTime:15/binary-unit:8, RestBin/binary>>) -> {ok, {binary_to_list(DateTime), RestBin}};
parse_date_time(_)                                             -> {error, date_time}.

parse_host(<<$ ,RestBin/binary>>) -> 
    {ok, {[Host], RestList}} = fs_lists:index_tokens(binary_to_list(RestBin), 1, " "), 
    {ok, {Host, RestList}};
parse_host(_) -> 
    {error, host}.
