-module(small_funs).

-export([bad/2, good/2]).

bad(UserEmail, Message) ->
  User =
    case users:find_by_email(UserEmail) of
      notfound ->
        users:new_with_email(UserEmail);
      FoundUser ->
        FoundUser
    end,
  
  EscapedMessage = message_utils:escape(Message),
  CleanMessage = bad_word_checker:clean(EscapedMessage),

  db:store_message(User, CleanMessage),
  
  DeviceIds = user:get_devices(User),
  lists:foreach(
    fun(DeviceId) ->
      case devices:get_device(DeviceId) of
        notfound -> ok;
        Device ->
          case device:get_push_info(Device) of
            {apns, Token} ->
              ApnsMsg = apns:build_message(CleanMessage),
              apns:send_msg(Token, ApnsMsg);
            {gcm, Token} ->
              GcmMsg = gcm:new_message(CleanMessage),
              gcm:send_message(Token, GcmMsg);
            _ -> ok
          end
      end
    end, DeviceIds).

good(UserEmail, Message) ->
  User = find_or_create_user(UserEmail),
  CleanMessage = clean_message(Message),

  db:store_message(User, CleanMessage),

  deliver_message(User, CleanMessage).


find_or_create_user(UserEmail) ->
  case users:find_by_email(UserEmail) of
    notfound ->
      users:new_with_email(UserEmail);
    FoundUser ->
      FoundUser
  end.

clean_message(Message) ->
  EscapedMessage = message_utils:escape(Message),
  bad_word_checker:clean(EscapedMessage).

deliver_message(User, Message) ->
  DeviceIds = user:get_devices(User),
  Devices =
    [devices:get_device(DeviceId) || DeviceId <- DeviceIds],
  lists:foreach(
    fun(notfound) -> ok;
       (Device) -> send_message(device:get_push_info(Device), Message)
    end, Devices).

send_message({apns, Token}, Message) ->
  ApnsMsg = apns:build_message(Message),
  apns:send_msg(Token, ApnsMsg);
send_message({gcm, Token}, Message) ->
  GcmMsg = gcm:new_message(Message),
  gcm:send_message(Token, GcmMsg);
send_message(_, _) -> ok.
