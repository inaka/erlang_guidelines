-module(spaghetti).

-export([bad/0, good/0]).

bad() ->
  Client = active_user:get_current_client(),
  [binary_to_list(Org)
   || Org <- autocomplete_db:members(
              case Client of
                home_client ->
                  <<"our:organizations">>;
                aperture_science ->
                  <<"client:", (prefix_for(aperture_science))/binary, ":orgs">>;
                wayne_ents ->
                  <<"client:", (prefix_for(wayne_ents))/binary, ":orgs">>
              end)].

good() ->
  Client = active_user:get_current_client(),
  RawOrgs = autocomplete_db:members(client_ac_key(Client)),
  [binary_to_list(Org) || Org <- RawOrgs].

client_ac_key(home_client) -> <<"our:organizations">>;
client_ac_key(Client) ->
  Prefix = prefix_for(Client),
  <<"client:", Prefix/binary, ":orgs">>.

prefix_for(aperture_science) -> <<"as">>;
prefix_for(wayne_ents) -> <<"we">>.
