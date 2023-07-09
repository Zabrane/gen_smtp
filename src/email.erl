%% From: https://gist.github.com/seriyps/6319602

%% Send plaintext email using gen_smtp https://github.com/Vagabond/gen_smtp
%% This function sends email directly to receiver's SMTP server and don't use
%% MTA relays.
%%
%% . Example plaintext email:
%%  > Mail = mail_plain(<<"Bob <sender@example.com>">>,
%%                      <<"Alice <receiver@example.com>">>,
%%                      <<"The mail subject">>,
%%                      <<"The mail body">>),
%%  > send_email(Mail).
%%
%% . Send to Multi-Recipient:
%%  > ToList = <<"receiver1@example.com,receiver2@example.com">>,
%%  or:
%%  > ToList = <<"<receiver1@example.com>,<receiver2@example.com>">>,
%%  or:
%%  > ToList = <<"Alice <receiver1@example.com>,\"Fion\" <receiver2@example.com>">>,
%%    ...
%%  > send_email(Mail).

%%
%% . Example email with image attachment:
%% > ImgName = <<"image.jpg">>,
%% > {ok, ImgBin} = file:read_file(ImgName),
%% > Mail = mail_with_attachments(<<"sender@example.com">>,
%%                                <<"receiver@example.com">>,
%%                                <<"Photos">>,
%%                                <<"See photo in attachment">>,
%%                                [{ImgName, mimerl:extension(ImgName), ImgBin}]),
%% > send_email(Mail).

-module(email).

-export([ send_email/1,
          mail_plain/4, mail_plain/5,
          multi_mail_plain/4,
          mail_with_attachments/6,
          multi_mail_with_attachments/5,
          extract_addr_rfc822/1 ]).


-type email() ::
  {string() | binary(), [string() | binary(), ...], string() | binary() | function()}.

-type email_ret() :: {ok, binary()} | {error, atom(), any()} | {error, any()}.

-spec send_email(email()) -> email_ret().
send_email({From, [To | _], _Body} = Email) ->
    [_, FromDomain] = split_addr(From),
    [_, ToDomain]   = split_addr(To),
    Options         = [ {relay, binary_to_list(ToDomain)}
                      %% , {port, 1465}
                        %% , {ssl, true}
                      %% , {username, "me@gmail.com"}
                      %% , {password, "mypassword"}
                      , {tls, if_available}
                      , {hostname, FromDomain} ],
    send_email(Email, Options).

send_email(Email, Options) ->
    case gen_smtp_client:send_blocking(Email, Options) of
        Id when is_binary(Id) ->
            {ok, binstr:chomp(Id)};
        Ret ->
            Ret
    end.

-spec mail_plain(binary(), binary(), binary(), binary()) -> email().
mail_plain(From, To, Subject, Body) ->
    mail_plain(From, To, Subject, Body, []).

-spec mail_plain(binary(), [binary()], binary(), binary(), Opts) -> email() when
      Opts :: [{headers, [{binary(), binary()}]}].
mail_plain(From, To, Subject, Body, Opts) ->
    AddHeaders = proplists:get_value(headers, Opts, []),
    Mimemail =
        {<<"text">>, <<"plain">>,
         [
          {<<"From">>, From},
          {<<"To">>, To},
          {<<"Subject">>, Subject},
          {<<"Content-Type">>, proplists:get_value(<<"Content-Type">>, AddHeaders, <<"text/plain; charset=utf-8">>)}
          | proplists:delete(<<"Content-Type">>,  AddHeaders)],
         %%[{<<"transfer-encoding">>, <<"base64">>}],
         #{transfer_encoding => <<"base64">>},
         Body},
    FromAddr = extract_addr_rfc822(From),
    {FromAddr, extract_addr_rfc822(To), mimemail:encode(Mimemail)}.

%% Send the same email to multiple recipients by isolate each one
-spec multi_mail_plain(binary(), [binary()], binary(), binary()) -> [email_ret()].
multi_mail_plain(From, ToList, Subject, Body) ->
    multi_mail_plain(From, ToList, Subject, Body, []).

multi_mail_plain(From, [To | Rest], Subject, Body, Acc) ->
    Mail = mail_plain(From, To, Subject, Body),
    multi_mail_plain(From, Rest, Subject, Body, [send_email(Mail) | Acc]);
multi_mail_plain(_, [], _, _, Acc) ->
    lists:reverse(Acc).


-spec mail_with_attachments(
        binary(), binary(),  binary(), binary(), binary(),
        [{Name :: binary(), MimeType :: binary(), Body :: binary()}]) -> email().

mail_with_attachments(From, To, Cc, Subject, Body, Attachments) ->
    MimeBody = {<<"text">>, <<"html">>,
                [{<<"Content-Type">>, <<"text/html;charset=utf-8">>},
                 {<<"Content-Transfer-Encoding">>, <<"quoted-printable">>},
                 {<<"Content-Disposition">>, <<"inline">>}],
                [],
                Body},
    MimeAttachments = [begin
                           [Ct1, Ct2] = binary:split(MimeType, <<"/">>),
                           {Ct1, Ct2,
                            [{<<"Content-Transfer-Encoding">>, <<"base64">>}],
                            [{<<"disposition">>, <<"attachment">>},
                             {<<"disposition-params">>,
                              [{<<"filename">>, Name}]}],
                            AtBody}
                       end
                       || {Name, MimeType, AtBody} <- Attachments],
    Opts = [{<<"From">>, From},
            {<<"To">>, To},
            {<<"Subject">>, Subject}],
                
    Mimemail = {<<"multipart">>,
               <<"mixed">>,
               add_copie(Opts, Cc),
               %%[],
               #{},
               [MimeBody | MimeAttachments]},
    FromAddr = extract_addr_rfc822(From),
    {FromAddr, extract_addr_rfc822(recipients(To, Cc)), mimemail:encode(Mimemail)}.
    
-spec add_copie(list(), binary()) -> list().
add_copie(Opts, []) ->
    Opts;
add_copie(Opts, <<>>) ->
    Opts;
add_copie(Opts, Cc) ->
    Opts ++ [{<<"Cc">>, Cc}].

-spec recipients(binary(), binary()) -> binary().
recipients(To, <<>>) ->
    To;
recipients(To, []) ->
    To;
recipients(To, Cc) ->
    Sep = <<", ">>,
    <<To/binary, Sep/binary, Cc/binary>>.
    
    
%% Send the same email with attachment to multiple recipients by isolate each one
-spec multi_mail_with_attachments(binary(), [binary()], binary(), binary(),
      [{Name :: binary(), MimeType :: binary(), Body :: binary()}]) -> [email_ret()].
multi_mail_with_attachments(From, ToList, Subject, Body, Attachments) ->
    multi_mail_with_attachments(From, ToList, Subject, Body, Attachments, []).

multi_mail_with_attachments(From, [To | Rest], Subject, Body, Attachments, Acc) ->
    Mail = mail_with_attachments(From, To, <<>>, Subject, Body, Attachments),
    multi_mail_with_attachments(From, Rest, Subject, Body, Attachments, [send_email(Mail) | Acc]);
multi_mail_with_attachments(_, [], _, _, _, Acc) ->
    lists:reverse(Acc).



extract_addr_rfc822(Rfc822) ->
    case smtp_util:parse_rfc822_addresses(Rfc822) of
        {ok, [{_, Addr}]} ->
            [list_to_binary(Addr)];
        {ok, L} ->
            %% io:format(">>> L: ~p~n", [L]),
            lists:map(fun({_, Addr}) -> list_to_binary(Addr) end, L)
    end.

split_addr(MailAddr) ->
    binary:split(MailAddr, <<"@">>).
