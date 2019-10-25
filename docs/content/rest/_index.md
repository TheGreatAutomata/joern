+++
title="Using Joern via its REST API"
weight=3
+++

Joern and Ocular can be scripted with many different programming languages via the REST-based API server `CPG server`. It allows code property graphs to be created, loaded, and queried via HTTP requests. This allows any program that can send and receive HTTP requests to script Joern.

This is the right interface for integration of Joern with other services using small scripts, interactive shell-based querying of graphs, and for UIs.

## Launching Joern Server

You can start the CPG server for Joern by issuing the command

```
./joernd
```

in your installation directory. By default, this will spawn a Web server on port 8080.


**Upon starting the server**, you can explore the API at:

```
http://localhost:8080/docs
```

Alternatively, you may fetch the raw Swagger YAML at:
```
http://localhost:8080/swagger.yaml
```

You should see the following:

{{<figure src="/docs/images/swagger.png">}}

## The Python library `cpgclientlib`

The following sample scripts shows how to create and query a CPG using Python.

```python
#!/usr/bin/env python3

from cpgclient.CpgClient import CpgClient

server = '127.0.0.1'
port = 8080
client = CpgClient(server, port)
client.create_cpg('/path/to/cpg')
methods = client.query('cpg.method.toJson')
print(methods)
```

For more information on the Python interface, please refer to

https://github.com/ShiftLeftSecurity/codepropertygraph/tree/master/cpgclientlib .

## Creating and loading CPGs with `cpg-create`

For convenience, we provide a Python script named `cpg-create` which instructs the CPG server to create and load a CPG:

```
cpg-create path/to/cpg
```

## Querying CPGs with `cpg-query`

You can query the CPG using `cpg-query`:

```
cpg-query "cpg.method.toJson"
```
