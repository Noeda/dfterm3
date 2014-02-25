Dfterm3 JSON/WebSocket API
================

This document describes the JSON API in Dfterm3 as it is used in a WebSocket
connection.

The version of this document is 0.1.0. The version semantics are as follows:

    MAJOR.MINOR.PATCH

If MAJOR is increased, then the API has changed in a backwards incompatible
manner.

If MINOR is increased, then the API has added something to the API but the
changes are backwards compatible.

if PATCH is increased, then the API is the same but bug fixes may have been
applied to this document.

It is undefined how dfterm3 will respond if client does not quite obey this
document.

Connection mechanics
====================

At the moment, only connections encapsulated in a WebSocket are considered. The
procedure of communication is as follows.

All the JSON sent are put in WebSocket text packets and that JSON should be a
single object. All the messages defined in this document are single objects.

Handshaking
-----------

After client connects to the server, the server will send a handshake to the
client.

    server -> client
    {
        "message":"handshake"
       ,"server":"dfterm3"
       ,"major":MAJOR
       ,"minor":MINOR
       ,"patch":PATCH
    }

MAJOR, MINOR and PATCH describe the protocol version the server is using, as
integers. In this version of protocol, the burden of adapting to different
versions is on client.

Client has to send a login message before any further communication can happen.

    client -> server
    {
        "message":"login"
       ,"identity":IDENTITY
       ,"password":PASSWORD       optional
    }

PASSWORD is a JSON string, if given.

IDENTITY shall take the form of one of the following objects.

    {
        "user":USERNAME
    }

    or

    {
        "guest":true
    }

These two forms are for users with accounts and users without accounts.

The server responds to a login message with the following message.

    server -> client
    {
        "message":"login_acknowledgement"
       ,"status":STATUS
       ,"notice":NOTICE
    }

Where STATUS is either the boolean true or false, for successful and
unsuccessful logins respectively. NOTICE is a string meant for humans to read
and should be told to the user logging in. This is used to tell why a login
might have been unsuccessful. In case of successful login, it can contain any
server notice that should be seen at login such as a MOTD.


