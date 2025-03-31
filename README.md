# exml

Re-implementation of [libxml2](https://gitlab.gnome.org/GNOME/libxml2) by Rust.\
This library is based on v2.11.8.

Some modules are now safe, but most major components are still unsafe.

For usage, please refer to the code under the `tests/` directory because `examples/` has not been implemented.

# Tests

All tests under the `tests/` directory are also based on original libxml2.\
These tests passes test under `test/` and `result/` directory in original libxml2.

If you try to run these codes, please download test data from original libxml2 repository.

## Note
The test code for `schematron` is not included in the original v2.11.8 repository. (only the data exists.)  
For coverage, only this test is ported from `master` (probably v2.14) and the original `runtest.c` output for the existing data is added to the test as expected values.

# License

This library is provided under the MIT License.  
Please refer to `LICENSE` file.
