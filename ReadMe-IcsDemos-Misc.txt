ICS - Internet Component Suite - V9.3 - Delphi 7 to RAD Studio 12
=================================================================
(Aka FPIETTE's Components)

Revised: September 25, 2024
Release: V9.3
https://www.overbyte.eu/
https://wiki.overbyte.eu/
https://svn.overbyte.be/svn/


ICS is a free internet component library for all Delphi, C++Builder, BDS and RAD Studio versions.
It includes TCP, UDP, raw sockets, clients, servers, as well as all the main high level protocols
such as FTP, SMTP, POP3, NNTP, HTTP and more. ICS also supports SSL/TLS with the help of OpenSSL.

ICS contains many sample applications developed over 25 years, used to develop and test ICS for
Windows VCL and FMX applications.


Pre-Built Samples
-----------------

All the main ICS active samples are available as prebuilt executables, to
allow ease of testing without needing to install ICS and build them all.
They are available to download from the wiki pages:

https://wiki.overbyte.eu/wiki/index.php/Main_Page

as four separate zip files split into clients, servers, tools and miscellaneous
samples.  All are built with Delphi 11.3 and those SSL/TLS are built with
OpenSSL embedded to needing external DLLs to be distributed.  The zips include a
few extra SSL certificate and other files to support the samples.

These samples should not treated as commercial applications, they are merely to
illustrate the type of applications the ICS components can be used to create.


ICS Miscellaneous Samples
-------------------------

OverbyteIcsBinCliDemo        Simple TCP client to receive binary and text data for OverbyteIcsTcpSrv
OverbyteIcsCliDemo           Simple TCP client for OverbyteIcsTcpSrv, IPV4 only
OverbyteIcsHttpPost          HTTP web POST using THttpCli component, works with all HTTP server samples
OverbyteIcsHttpTst           HTTP web GET using THttpCli component, tests all commands
OverbyteIcsMimeDemo          MIME email decoding, attached files are extracted using TMimeDecodeW
OverbyteIcsSimpleSslCli      Simple SSL TCP client using TSslWSocket component
OverbyteIcsSimpleSslServer   Simple SSL TCP server using TSslWSocket component
OverbyteIcsSocksTst          SOCKS and HTTP tunnel proxy client testing using TSslWSocket component
OverbyteIcsSnmpCliTst        SNMP (simple network management protocol) using TSnmpCli component
OverbyteIcsSysLogClientDemo  SysLog client using TSysLogClient component
OverbyteIcsSysLogServerDemo  SysLog server using TSysLogServer component
OverbyteIcsTcpSrv            Basic TCP server without client forms, uses TWSocketServer, IPv4 only
OverbyteIcsTcpSrvIPv6        Basic TCP server without client forms, uses TWSocketServer, IPv4/IPV6
OverbyteIcsTelnetClient      Telnet terminal client using TnEmulVT component
OverbyteIcsThrdSrv           Multithreaded TCP server, banner sent in main thread, uses TWSocketServer component
OverbyteIcsThrdSrvV2         Multithreaded TCP server, banner sent in worker thread, uses TWSocketServer component
OverbyteIcsThrdSrvV3         Multithreaded TCP server, uses TWSocketThrdServer component
OverbyteIcsTimeTst           SNTP time protocol client and server, using TIcsTimeClient and TIcsTimeServer components
OverbyteIcsWebAppServer      HTTPS web server with sessions, uses THttpAppSrv component


Getting Started with ICS
------------------------

ICS has a large number of sample application whose primary purpose is to test
all the components and to learn about using those components and how to use
them in your own applications.  There are often several samples for a single
protocol with different purposes, so this section should help get you started
choosing the components and samples for your internet project.

ICS often offers low and high level versions of components, the former allow
your application to send the various commands used by the protocol but you
need to send those commands in the correct order often dependent upon the
result from earlier commands, so you need to understand the protocol, but
have control over the commands.  The high level components are quicker and
easier to implement because they hide most of the protocol and offer complex
methods instead such as download a file, they often include extra functionality.

Historically, most ICS components are available on non-SSL and SSL versions,
these notes assume you are using SSL/TLS components which are often essential
today.  Note most low level component need SSL/TLS adding using an SslContext
and need SSL certificate chain checking added to applications, while the
higher level components mostly already include the SslContext and chain
checking and hide much of the SSL/TLS complexity making them faster to
implement and easier to maintain as SSL changes.


Simple TCP Socket Client
------------------------

TSslWSocket in unit OverbyteIcsWSocket is the root of most other ICS
components opening a socket to either connect to a remote server, or to
listen for connections from a remote server.  The component always opens
a socket by IP address, but will look-up that IP address from a host
name if required, or provide a reverse look-up of host or domain name
from an IP address. TSslWSocket sends or receives a stream of 8-bit
binary characters, but does have methods to send and receive lines by
checking or sending a CRLF line ending, which is the Telnet protocol,
used for the headers all most other high level protocols like HTTP,
FTP, SMTP, etc.  TSslWSocket can use TCP or UDP transmission, most
protocols use TCP, except DNS and SNMP. TSslWSocket can be tested using
samples OverbyteIcsSimpleSslCli, OverbyteIcsCliDemo, OverbyteIcsBinCliDemo,
OverbyteIcsUdpLstn, OverbyteIcsUdpSend and many others.  Note TSslWSocket
requires an SslContext for SSL configuration.


Simple TCP Socket Server
------------------------

TSslWSocketServer in unit OverbyteIcsWSocketS is the main socket server
accepting a few thousand remote clients using multiple IP addresses and
ports, and separately allowing data to be sent and received from those
remote clients, all in a single thread.  Applications need to derive
a client from TSslWSocketClient into which the required functionality
is added.  TSslWSocketServer supports using collections of IcsHosts
properties. This allows the server to listen on multiple IP addresses
and ports at the same time with different SSL/TLS certificates for each
host using built-in SslContexts, will automatically create self signed
SSL/TLS certificates so the server can start, and will them order free
SSL/TLS certificates from Let's Encrypt (provided running on the public
internet), and re-order them every three months before they expire.

TSslWSocketServer is mostly tested using the ICS HTTP and FTP servers,
but there are other samples, OverbyteIcsSimpleSslServer, OverbyteIcsTcpSrv,
OverbyteIcsTcpSrvIPV6, OverbyteIcsThrdSrv, OverbyteIcsThrdSrvV2, etc.

There is also a threaded version TSslWSocketThrdServer in unit
OverbyteIcsWSocketTS where each client is created with a separate thread
to avoid blocking on high load servers.  Beware this server does not yet
support IcsHosts and multiple IP addresses, nor is there a web server
using it.  It is tested using sample OverbyteIcsThrdSrvV3.


Receiving Email, POP3 Client
----------------------------

The unit OverbyteIcsMimeDec contains functions for decoding MIME encoded
emails, tested with sample OverbyteIcsMimeDemo.


Network Diagnostic Tools
------------------------

TSnmpCli in unit OverbyteIcsSnmpCli does SNMP (simple network management
protocol), tested by sample OverbyteIcsSnmpCliTst.

TSysLogClient in unit OverbyteIcsSysLogClient send syslog packets, tested
by sample OverbyteIcsSysLogClientDemo.

TSysLogServer in unit OverbyteIcsSysLogServer receives syslog packets,
tested by sample OverbyteIcsSysLogServerDemo.

TIcsTimeClient and TIcsTimeServer in unit OverbyteIcsSntp support SNTP
for getting and setting the correct time over the internet, tested
using sample OverbyteIcsTimeTst.
