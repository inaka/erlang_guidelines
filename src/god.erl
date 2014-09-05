%%% @doc all of your db operations belong to us!
-module(god).

-export([create_user/1, create_user/2, create_user/3]).
-export([update_user/2, update_user/3]).
-export([delete_user/1]).
-export([create_post/1, create_post/2, create_post/3]).
-export([update_post/2, update_post/3]).
-export([delete_post/1]).
-export([create_comment/2, create_comment/3]).
-export([update_comment/3, update_comment/4]).
-export([delete_comment/2]).

create_user(Name) -> create_user(Name, undefined).

create_user(Name, Email) -> create_user(Name, Email, undefined).

create_user(Name, Email, Phone) ->
  some_db:insert(users, [{name, Name}, {email, Email}, {phone, Phone}]).

update_user(Name, Changes) ->
  some_db:update(users, [{name, Name}], Changes).

update_user(Name, Key, Value) ->
  update_user(Name, [{Key, Value}]).

delete_user(Name) ->
  some_db:delete(users, [{name, Name}]).

create_post(Text) -> create_post(Text, undefined).

create_post(Text, Title) -> create_post(Text, Title, undefined).

create_post(Text, Title, Image) ->
  some_db:insert(posts, [{text, Text}, {title, Title}, {image, Image}]).

update_post(Text, Changes) ->
  some_db:update(posts, [{text, Text}], Changes).

update_post(Text, Key, Value) ->
  update_post(Text, [{Key, Value}]).

delete_post(Text) ->
  some_db:delete(posts, [{text, Text}]).

create_comment(PostId, Text) -> create_comment(PostId, Text, undefined).

create_comment(PostId, Text, Image) ->
  some_db:insert(comments, [{post_id, PostId}, {text, Text}, {image, Image}]).

update_comment(PostId, CommentId, Changes) ->
  some_db:update(comments, [{post_id, PostId}, {id, CommentId}], Changes).

update_comment(PostId, CommentId, Key, Value) ->
  update_comment(PostId, CommentId, [{Key, Value}]).

delete_comment(PostId, CommentId) ->
  some_db:delete(comments, [{post_id, PostId}, {id, CommentId}]).

