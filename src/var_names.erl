-module(var_names).

-export([bad/1, good/1]).

bad(OrganizationToken) ->
  OID = organization:get_id(OrganizationToken),
  OID.

good(OrgToken) ->
  OrgID = organization:get_id(OrgToken),
  OrgID.
