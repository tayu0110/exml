# exml

Re-implementation of [libxml2](https://gitlab.gnome.org/GNOME/libxml2) by Rust.\
This library is based on `v2.11.8`.

Currently, all codes are just translated from original C codes.\
Therefore, almost of all public interfaces are unsafe.

# `tests/`

All tests under the `tests/` directory are also based on original `libxml2`.\
These tests passes test under `test/` and `result/` directory in original
`libxml2`.

If you try to run these codes, please download test data from original `libxml2`
repository.

# License

I would like to publish it at MIT in the future, but it is not ready for
publication at this time, so I will not grant a license currently.
