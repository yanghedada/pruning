## Database
- uid		Integer
- username	string
- md5password	hex string
- key		hex string, used for AES encryption (generated from username/pwd)

## API
--------------------------------------------------------------------------------

### REGISTER
(server respond as soon as possible)

**/register**

client:
```json
	{
		"username": "theusername(RSA)",
		"password": "md5'ed password(RSA)"
	}
```

server: store `(username, md5'ed password, token)` in database where 
token is generated from username and password. and respond.

```json
	{
		"code": "200",
		"msg": ""
	}
```	
on success,
```json
	{
		"code": "400, 422",
		"msg": "bad request, or username already existed"
	}
```
invalid json/invalid field or username existed

--------------------------------------------------------------------------------

### LOGIN

(server respond as soon as possible)

**/login**

client:
```json
	{
		"username": "theusername(RSA)",
		"password": "md5'ed password(RSA)",
		"devicename": "Nexus, Huawei, Xiaomi, Chromium...(RSA)"
	}
```
server: autheticate by querying database, return
```json
	{
		"code": "200",
		"token": "aes encrypted token(possibly base64'ed)"
		"msg": "empty"
	}
```
on success,
```json
	{
		"code": "400, 422",
		"msg": "bad request or wrong username/pwd"
	}
```
invalid json or wrong username / pwd

then the server will add this token to a memory/physical database as
tuple: ~~`(uid, connection, token, devicename)`~~(see the datatype at end)
token is used to identify different devices owned by one account

the client should keep the token carefully and include it to communicate
with server afterwards.

--------------------------------------------------------------------------------

### POST

(server response as soon as possible)

**/post**

client:
```json
	{
		"token": "previously gotten token(RSA)",
		"data": "rsa encrypted data (RSA)"
	}
```
server: server add data to message queue which is to be send
```json
	{
		"code": "200",
		"msg": ""
	}

on success or
```json
	{
		"code": "400",
		"msg": "bad request, etc"
	}
```

--------------------------------------------------------------------------------
### SYNC

(server hold the connection, send back message whenever the message queue is not empty)

**/sync**

client:
```json
	{
		"token": "previously gotten token(RSA)"
	}
```
server: when there's new message, send that to client
```
	{
		"msg": "aes encrypted message, (AES and base64)",
		"msgid": "a number indicating the msg id, (AES and base64)",
		(optional)"time": "message time, (maybe added later)"
	}
```
then, if the client received the message, send an ACK:
client:
```json
	{
		"msgid": "received msg id",
		"code": "ok"
	}
```
after the server gets this ack, it deleted the msg stored and proceed 
to the next one(wait when msg queue empty)
		
It is the client's duty to start the ping frame (to keep the connection),
and reconnect whenever it detects disconnect. When a disconnect happens,
the server will keep the unsend message and wait for the same token to
connect back, then send the remaining message.

--------------------------------------------------------------------------------

### LOGOUT

(server respond as soon as possible)

**/logout**

client:
```json
	{
		"token": "previously gotten token (RSA)"
		"(optional)reason": "reason, (maybe added later)"
	}
```
then server send back an ack.
server:
```json
	{
		"code": "200"
	} on success or
	{
		"code": "400"
		"msg": "bad request"
	}
```
when bad formatted json or no such token then the server clears 
all the message related to that token.

--------------------------------------------------------------------------------

## datatypes
- `Map token uid`
- `Map uid [tokens]`
- `Map token MessageQueue` -> memory as MVar

so when the server restart, all messages lost, but tokens is restored
