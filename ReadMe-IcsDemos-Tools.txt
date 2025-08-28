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


ICS Tool Samples
----------------

OverbyteIcsBatchDnsLookup    Multiple DNS lookups using TIcsDomainNameCache component
OverbyteIcsJoseTst           SSL Json Object Signing (Jose) Demos, used for REST and OAUTH2
OverbyteIcsNetMon            Internet Packet Monitoring Components, using TIcsMonSocket and TIcsMonPCap
OverbyteIcsNetTools          Network Tools Demo, uses all the main IP Helper functions and many other components
OverbyteIcsNsLookup          DNS lookups using the TDnsQuery component, all request types.
OverbyteIcsPemTool           SSL/TLS Certificate Tool, process certificates using TX509Base and TX509List
OverbyteIcsPingTst           Trace route and pinging using the TPing and TPingThread components
OverbyteIcsWhoisCliTst       Whois client, looks up servers automatically, using TIcsWhoisCli component
OverbyteIcsWmiTst            WMI functions, general purpose, update Windows IP addresses and DNS Server records
OverbyteIcsX509CertsTst      Download SSL X509 certificates from Let's Encrypt using TSslX509Certs component


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


Create, Order or Review SSL/TLS Certificates
--------------------------------------------

ICS contains many functions for processing SSL/TLS X509 certificates and
private keys. TX509Base in unit OverbyteIcsWSocket may contain and server
or client certificate, private key and one of more intermediate
certificates, and has properties to display most of the certificate
elements, all tested by sample OverbyteIcsPemtool.  TX509List contains
multiple certificates, typically a root store loaded from a PEM file.
TMsX509List will load certificates from any Windows Certificate Store
including private keys. TSslCertTools in unit OverbyteIcsSslX509Utils can
read, create and save certificates, private keys, certificate requests and
sign requests as a certificate authority.

The OverbyteIcsPemtool sample can be used to create certificates and
private keys in various RSA, EC and other formats, create certificate
requests and sign requests as a certificate authority, and convert
certificate files between different formats, by reading as one and saving
as another, also combining keys and certificates in a file.
Root stores and single certificates may be to be viewed.  Certificates
and private keys in Windows Stores may be viewed and deleted, and a PEM
or PFX certificate bundle installed into any Windows Store.

TSslX509Certs in unit OverbyteIcsSslX509Certs, tested by sample
OverbyteIcsX509CertsTst which automatically downloads SSL/TLS X509
certificates from various issuers, including free certificates from Let's
Encrypt, and  commercial certificates from CertCentre AG. Supports ACME
V2 protocol, and REST protocols for specific vendors.  Domain and DNS
validated certificates should generally  be issued without intervention,
other commercial certificates may take days to be approved. This unit may
be added to ICS server applications using IcsHosts for automatic ordering,
while the sample may be separately used to order certificates manually,
including DNS validated wildcard certificates from Let'S Encrypt.  All orders
are kept in a database to allow automatic or manual re-ordering before expiry.


Lookup Domain Names, DNS
------------------------

Simple DNS host look-ups using the DNS servers configured for Windows are
done using the DnsLookup method in TSslWSocket and also the ReverseDnsLookup
method, both fire an event with potentially multiple results, tested by
sample OverbyteIcsDnsLook.  These methods are used by almost all ICS
components, although they currently only use the first IPv4 or IPv6 result
returned, if more than one.

TDnsQuery in unit OverbyteIcsDnsQuery allows more complex DNS requests to
be made to specific DNS servers to get all DNS records such as MX or TXT,
tested using sample OverbyteIcsNsLookup.  It includes a list of public DNS
servers including Google, Cloudfare, OpenDNS and others, and will access
these sequentially if one does not respond.  TDnsQueryHttps in unit
OverbyteIcsSslHttpRest adds DNS over HTTPS for secure lookups. Also tested
with sample OverbyteIcsNetTools.

TIcsDomNameCache and TIcsDomNameCacheHttps cache forward and reverse DNS lookup
requests, mainly for diagnostic components but also for servers logging remote
access.  May be configured to use Windows lookup, UDP/TCP using TDnsQuery or
HTTPS, testing using samples OverbyteIcsBatchDnsLookup and OverbyteIcsNetTools.

Unit OverbyteIcsWmi contains a number of functions for accessing a Windows
DNS Server (Windows Server 2012 and later) to list DNS zones and zone
records, and to add zone records, tested by sample OverbyteIcsWmiTst.  The
functions are also used by sample OverbyteIcsX509CertsTst to add DNS records
for the ACME DNS challenge.


Network Diagnostic Tools
------------------------

TPing and TPingThread in unit OverbyteIcsPing is used to ping any host to see
if it's available on the internet, note some hosts may deliberately not reply,
tested by samples OverbyteIcsPingTst and OverbyteIcsNetTools which both include
trace route.

TIcsWhoisCli in unit OverbyteIcsWhoisCli makes Whois requests to get
details for the registrations of domain names and IP address ranges,
tested by samples OverbyteIcsWhoisCliTst and OverbyteIcsNetTools. The
component has a large list of Whois servers for various countries
around the world.

TIcsMonSocket in OverbyteIcsMonSock provides internet packet monitoring
using raw sockets.  TIcsMonPcap in OverbyteIcsMonPcap provides internet
monitoring using the Npcap NDIS driver.  There are both tested using
sample OverbyteIcsNetMon which is similar to the WireShark diagnostic tool
and can be used to monitor internet packets on a LAN, with filtering
using TIcsMonFilterClass to include or exclude IPs, port or protocols.

TIcsIpChanges in OverbyteIcsIpHlpApi monitors IP address changes and calls
an event for new IPs configured or old ones removed.  TIcsNeighbDevices
in OverbyteIcsIpHlpApi builds a historic LAN MAC device and IPv4 and IPv6
address table using ARP, neighbourhood and IP range scanning with reverse
host lookup. Both are tested with sample OverbyteIcsNetTools which also
uses several other IpHlp functions including IP Connections list, Network
Adaptors and Interfaces, IP Routing and Path tables, ARP tables and Network
Statistics.

