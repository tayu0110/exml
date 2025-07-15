# exml
Re-implementation of [libxml2](https://gitlab.gnome.org/GNOME/libxml2) by Rust.\
This library is based on v2.11.8.

Most modules are now safe, but a part of major modules are still unsafe.

For usage, please refer to the code under the `tests/` directory because `examples/` has not been implemented.

# Supported features
It is recommended to use the default features.  
Since the implementation is still insufficient, it may not be possible to build if some features are disabled.

| feature            | description                                                               |
|:-------------------|:--------------------------------------------------------------------------|
| c14n               | Corresponds to `XML_WITH_C14N`                                            |
| catalog            | Corresponds to `XML_WITH_CATALOG`                                         |
| html               | Corresponds to `XML_WITH_HTML`                                            |
| http               | Corresponds to `XML_WITH_HTTP`                                            |
| sax1               | Corresponds to `XML_WITH_SAX1`                                            |
| schema             | Corresponds to `XML_WITH_SCHEMAS`                                         |
| schematron         | Corresponds to `XML_WITH_SCHEMATRON`                                      |
| xinclude           | Corresponds to `XML_WITH_XINCLUDE`                                        |
| xpath              | Corresponds to `XML_WITH_XPATH`                                           |
| xpointer           | Corresponds to `XML_WITH_XPTR`                                            |
| libxml_automata    | Corresponds to `XML_WITH_AUTOMATA`                                        |
| libxml_debug       | Corresponds to `XML_WITH_DEBUG`                                           |
| libxml_iso8859x    | Corresponds to `XML_WITH_ISO8859X`                                        |
| libxml_output      | Corresponds to `XML_WITH_OUTPUT`                                          |
| libxml_pattern     | Corresponds to `XML_WITH_PATTERN`                                         |
| libxml_push        | Corresponds to `XML_WITH_PUSH`                                            |
| libxml_reader      | Corresponds to `XML_WITH_READER`                                          |
| libxml_regexp      | Corresponds to `XML_WITH_REGEXP`                                          |
| libxml_tree        | Corresponds to `XML_WITH_TREE`                                            |
| libxml_unicode     | Corresponds to `XML_WITH_UNICODE`                                         |
| libxml_valid       | Corresponds to `XML_WITH_VALID`                                           |
| libxml_writer      | Corresponds to `XML_WITH_WRITER`                                          |
| libxml_xptr_locs   | Corresponds to the behavior when `LIBXML_XPTR_LOCS_ENABLED` is enabled.   |

## DOM Support
The `dom` module is in implementation for the purpose of replacing the `tree` module.

The `tree` APIs require the users to manage memory, making it difficult to handle safely, but the `dom` module introduces memory management by reference counters, making it safer to handle.\
In addition, by providing APIs that conform to the DOM specification, users can manipulate the document tree in a more generally accepted way.

Since the `tree` module is an important module used in most aspects of libxml, it is expected that the replacement will be completed in a later version of this crate as well.

# Tests
## libxml2 tests
`runsuite`, `runtest`, `runxmlconf`, `test_threads`, `testdict`, `testlimits` and `testrecurse` are based on original libxml2.\
These tests pass tests under `test/` and `result/` directory.

Some tests under `test/` and `result/` are modified according to the specification, but basically all are libxml2 artifacts.

## DOM Test Suite
`dom_test_suite` is a re-implementation of the tests provided by W3C for Java for Rust.\
In the current status, most of the methods up to DOM Level 2 Core pass the tests.

You would get the original tests by running `resources/get-dom-test-suite.ts`. Note that the current W3C page seems to avoid bots, so you will need to manually get the cookie from your browser.\
I have inserted a one second delay for each resource fetch in light of the load on the provider's server, but please be careful not to run it unnecessarily.

I have also prepared `resources/generate-test-code.ts` for test generation, but please don't expect it to be good.

The scripts are intended to be executed using `deno`.

## Note
The test code for `schematron` is not included in the original v2.11.8 repository. (only the data exists.)  
For coverage, only this test is ported from `master` (probably v2.14) and the original `runtest.c` output for the existing data is added to the test as expected values.

# License
This library is provided under the MIT License.  
Please refer to `LICENSE` file.
