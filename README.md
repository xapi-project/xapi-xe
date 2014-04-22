xe
==

The [xapi](https://github.com/xapi-project/xen-api) command-line interface.

Simple usage:
```
xe -s <host or IP> -u root -pw <password> <command> [key0=val0] [key1=val1]
```

It is recommended to create a file ${HOME}/.xe which contains
```
server=192.168.100.12
username=root
password=password
```
rather than passing credentials on the command-line.

Example commands (assuming a populated ${HOME}/.xe):
```
xe vm-list
VM=$(xe vm-install template-name=<name> new-name-label=myvm)
xe vm-start vm=$VM
```

Implementation notes
====================

This is a simple tool which remotes command-lines to a xapi server,
which returns simple commands like "print this text" or "upload this
file". This tool does not use the XenAPI directly.
