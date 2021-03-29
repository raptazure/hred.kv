# Hredis

A multi-threaded, TCP, key-value store inspired by Redis.

- Build and run:

```sh
$ stack build
$ stack run
```
- Test it out with `redis-cli`:

```sh
$ redis-cli -p 7777
127.0.0.1:7777> set name venti
OK
127.0.0.1:7777> get name
"venti"
```

- Test the performance with something silly:

```sh
$ time redis-cli -r 10000 get name
```
