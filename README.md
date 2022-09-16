# gumbo Erlang

## FAQ

### How to install gumbo on OpenBSD?

```sh
pkg_add gumbo
```

 * https://openports.se/www/gumbo

### How to compile gumbo on OpenBSD?

```sh
pkg_add automake autoconf git gnumake
export AUTOCONF_VERSION=# based on your autoconf version installed
export AUTOMAKE_VERSION=# based on your automake version installed
git clone https://github.com/google/gumbo-parser
cd gumbo-parser
./autogen.sh
./configure
gmake
```

 * https://github.com/google/gumbo-parser
 * https://matze.github.io/clib-doc/gumbo-parser/index.html
