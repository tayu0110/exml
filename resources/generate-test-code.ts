/// deno run -A generate-test-code.ts && rustfmt __dom_test_suite.rs && mv __dom_test_suite.rs ../tests/
import { DOMParser, Element, Node } from "npm:@xmldom/xmldom";
import * as path from "jsr:@std/path";
import { snakeCase } from "jsr:@luca/cases";

let buffer: string = `#![allow(unreachable_code)]
#[cfg(test)]
mod dom_test_suite {
    use exml::dom::*;
    use exml::dom::document::*;
    use exml::dom::node::*;
    use exml::dom::character_data::*;
`;

const TEST_SUITE: [string, string[]][] = [
    ["level1", ["core" /* "html" */]],
    ["level2", ["core" /* "events", "html" */]],
    ["level3", ["core" /* "events", "ls", "validation", "xpath" */]],
];

Deno.chdir("./DOM-Test-Suite/tests");
for (const [d1, d] of TEST_SUITE) {
    Deno.chdir(`./${d1}`);
    buffer += `mod ${d1} {use super::*;`;
    for (const d2 of d) {
        Deno.chdir(`./${d2}`);
        buffer += `mod ${d2} {use super::*;`;

        for (const entry of Deno.readDirSync("./")) {
            if (
                entry.isFile && entry.name.endsWith(".xml") &&
                entry.name !== "alltests.xml" &&
                entry.name !== "metadata.xml"
            ) {
                console.log(`start read ${entry.name}`);
                const doc = new DOMParser().parseFromString(
                    Deno.readTextFileSync(`./${entry.name}`),
                    "text/xml",
                );
                buffer += `\n// ${entry.name}\n`;
                buffer += `#[test]fn test_${
                    snakeCase(path.basename(entry.name).split(".")[0])
                }() {`;

                const root = doc.documentElement;
                if (root) {
                    let child = root.firstChild;
                    while (child) {
                        if (
                            child.nodeType === child.ELEMENT_NODE &&
                            child.nodeName !== "metadata"
                        ) {
                            const elem = child as unknown as Element;
                            if (child.nodeName === "var") {
                                const name = elem.getAttribute("name");
                                const type = elem.getAttribute("type");
                                const value = elem.getAttribute("value");
                                if (name && type) {
                                    if (type === "Document") {
                                        buffer += `let mut r#${
                                            snakeCase(name)
                                        }: DocumentRef;`;
                                    } else if (
                                        type === "List" && elem.firstChild
                                    ) {
                                        buffer += `let mut r#${
                                            snakeCase(name)
                                        } = vec![`;
                                        let child: Node | null =
                                            elem.firstChild;
                                        while (child) {
                                            const value = child.textContent;
                                            if (
                                                child.nodeName === "member" &&
                                                value
                                            ) {
                                                if (value.startsWith('"')) {
                                                    buffer += `${value},`;
                                                } else {
                                                    buffer += `${
                                                        snakeCase(value)
                                                    },`;
                                                }
                                            }
                                            child = child.nextSibling;
                                        }
                                        buffer += `]; // type: ${type}`;
                                    } else if (value) {
                                        buffer += `let mut r#${
                                            snakeCase(name)
                                        } = ${value}; // type: ${type} `;
                                    } else {
                                        buffer += `let mut r#${
                                            snakeCase(name)
                                        }; // type: ${type} `;
                                    }
                                }
                            } else if (child.nodeName === "load") {
                                const vr = elem.getAttribute("var");
                                const href = elem.getAttribute("href");
                                if (vr) {
                                    buffer += `r#${
                                        snakeCase(vr)
                                    } = todo!(); // ${href}.xml `;
                                }
                            } else if (child.nodeName === "item") {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const index = elem.getAttribute("index");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.item(${index}).unwrap();`;
                                }
                            } else if (child.nodeName === "assertEquals") {
                                const actual = elem.getAttribute("actual");
                                const expected = elem.getAttribute("expected");
                                if (actual && expected) {
                                    buffer += `assert_eq!(r#${
                                        snakeCase(actual)
                                    }, ${
                                        expected?.startsWith('"')
                                            ? expected
                                            : snakeCase(expected)
                                    });`;
                                }
                            } else if (child.nodeName === "assertURIEquals") {
                                const actual = elem.getAttribute("actual");
                                const expected = elem.getAttribute("expected");
                                if (actual && expected) {
                                    buffer += `assert_eq!(r#${
                                        snakeCase(actual)
                                    }, ${
                                        expected?.startsWith('"')
                                            ? expected
                                            : `r#${snakeCase(expected)}`
                                    });`;
                                }
                            } else if (
                                child.nodeName === "assertTrue" ||
                                child.nodeName === "assertFalse"
                            ) {
                                const actual = elem.getAttribute("actual");
                                if (actual) {
                                    buffer += `assert!(${
                                        child.nodeName === "assertFalse"
                                            ? "!"
                                            : ""
                                    }r#${snakeCase(actual)});`;
                                } else {
                                    buffer += `\n// unimplemented: `;
                                }
                            } else if (child.nodeName === "assertNull") {
                                const actual = elem.getAttribute("actual");
                                if (actual) {
                                    buffer += `assert!(r#${
                                        snakeCase(actual)
                                    }.is_none());`;
                                } else {
                                    buffer += `\n// unimplemented: `;
                                }
                            } else if (child.nodeName === "splitText") {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const offset = elem.getAttribute("offset");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${
                                        snakeCase(child.nodeName)
                                    }(${offset}).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "getElementsByTagName"
                            ) {
                                const tagname = elem.getAttribute("tagname");
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.get_elements_by_tag_name(${tagname});`;
                                }
                            } else if (
                                child.nodeName === "getNamedItem" ||
                                child.nodeName === "removeNamedItem" ||
                                child.nodeName === "removeAttributeNode"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const vr = elem.getAttribute("var");
                                const name = elem.getAttribute("name") ||
                                    elem.getAttribute("newAttr") ||
                                    elem.getAttribute("oldAttr") ||
                                    elem.getAttribute("arg");
                                if (vr && obj && name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        name?.startsWith('"')
                                            ? name
                                            : `r#${snakeCase(name)}`
                                    }.into()).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "getAttributeNode"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const vr = elem.getAttribute("var");
                                const name = elem.getAttribute("name") ||
                                    elem.getAttribute("newAttr") ||
                                    elem.getAttribute("oldAttr") ||
                                    elem.getAttribute("arg");
                                if (vr && obj && name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        name?.startsWith('"')
                                            ? name
                                            : `r#${snakeCase(name)}`
                                    }).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "setAttributeNode" ||
                                child.nodeName === "setNamedItem" ||
                                child.nodeName === "setNamedItemNS"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const vr = elem.getAttribute("var");
                                const name = elem.getAttribute("newAttr") ||
                                    elem.getAttribute("arg");
                                if (vr && obj && name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        name?.startsWith('"')
                                            ? name
                                            : `r#${snakeCase(name)}`
                                    }.into()).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "setAttributeNodeNS"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const vr = elem.getAttribute("var");
                                const name = elem.getAttribute("newAttr") ||
                                    elem.getAttribute("arg");
                                if (vr && obj && name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        name?.startsWith('"')
                                            ? name
                                            : `r#${snakeCase(name)}`
                                    }.as_attribute().unwrap()).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "firstChild" ||
                                child.nodeName === "lastChild" ||
                                child.nodeName === "previousSibling" ||
                                child.nodeName === "nextSibling" ||
                                child.nodeName === "doctype" ||
                                child.nodeName === "documentElement" ||
                                child.nodeName === "ownerDocument" ||
                                child.nodeName === "ownerElement" ||
                                child.nodeName === "parentNode" ||
                                child.nodeName === "publicId" ||
                                child.nodeName === "systemId" ||
                                child.nodeName === "notationName"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const vr = elem.getAttribute("var");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}().unwrap();`;
                                }
                            } else if (
                                child.nodeName === "createEntityReference" ||
                                child.nodeName === "createAttribute" ||
                                child.nodeName === "createElement" ||
                                child.nodeName === "createAttribute" ||
                                child.nodeName === "getAttribute"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const name = elem.getAttribute("name") ||
                                    elem.getAttribute("tagName");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${
                                        snakeCase(child.nodeName)
                                    }(${name}.to_string()).unwrap();`;
                                } else if (obj) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${name}.to_string()).unwrap();`;
                                }
                            } else if (child.nodeName === "setAttribute") {
                                const obj = elem.getAttribute("obj");
                                const name = elem.getAttribute("name");
                                const value = elem.getAttribute("value");
                                if (obj && name && value) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${
                                        name.startsWith('"')
                                            ? name
                                            : `r#${snakeCase(name)}`
                                    }, ${
                                        value.startsWith('"')
                                            ? value
                                            : `r#${snakeCase(value)}`
                                    }).unwrap();`;
                                }
                            } else if (child.nodeName === "removeAttribute") {
                                const obj = elem.getAttribute("obj");
                                const name = elem.getAttribute("name");
                                if (obj && name) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${
                                        name.startsWith('"')
                                            ? name
                                            : `r#${snakeCase(name)}`
                                    }.into()).unwrap();`;
                                }
                            } else if (child.nodeName === "hasAttribute") {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const name = elem.getAttribute("name");
                                if (vr && obj && name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        name.startsWith('"')
                                            ? name
                                            : `r#${snakeCase(name)}`
                                    });`;
                                }
                            } else if (
                                child.nodeName === "setIdAttributeNode"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const name = elem.getAttribute("idAttr");
                                const value = elem.getAttribute("isId");
                                if (obj && name && value) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(r#${snakeCase(name)}, ${
                                        snakeCase(value)
                                    }).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "setIdAttribute"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const name = elem.getAttribute("name");
                                const is_id = elem.getAttribute("isId");
                                if (obj && name && is_id) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${
                                        name.startsWith('"')
                                            ? name
                                            : `r#${snakeCase(name)}`
                                    }, ${is_id}).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "createTextNode" ||
                                child.nodeName === "createDocumentFragment" ||
                                child.nodeName === "createComment"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const data = elem.getAttribute("data");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${(data
                                        ? (data.startsWith('"')
                                            ? data
                                            : `r#${snakeCase(data)}`)
                                        : "")});`;
                                }
                            } else if (
                                child.nodeName === "createCDATASection"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const data = elem.getAttribute("data");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${(data
                                        ? (data.startsWith('"')
                                            ? data
                                            : `r#${snakeCase(data)}`)
                                        : "")}).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "createProcessingInstruction"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const target = elem.getAttribute("target");
                                const data = elem.getAttribute("data");
                                if (vr && obj && target) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        target.startsWith('"')
                                            ? target
                                            : snakeCase(target)
                                    }, ${
                                        data
                                            ? (data.startsWith('"')
                                                ? `Some(${data})`
                                                : `Some(${snakeCase(data)})`)
                                            : "None::<Rc<str>>"
                                    }).unwrap();`;
                                }
                            } else if (
                                (child.nodeName === "nodeValue" &&
                                    elem.getAttribute("var")) ||
                                child.nodeName === "nodeName" ||
                                (child.nodeName === "value" &&
                                    elem.getAttribute("var")) ||
                                (child.nodeName === "namespaceURI" &&
                                    elem.getAttribute("var")) ||
                                (child.nodeName === "localName" &&
                                    elem.getAttribute("var")) ||
                                (child.nodeName === "prefix" &&
                                    elem.getAttribute("var")) ||
                                child.nodeName === "tagName" ||
                                child.nodeName === "name" ||
                                (child.nodeName === "documentURI" &&
                                    elem.getAttribute("var")) ||
                                (child.nodeName === "xmlEncoding" &&
                                    elem.getAttribute("var")) ||
                                (child.nodeName === "inputEncoding" &&
                                    elem.getAttribute("var")) ||
                                (child.nodeName === "xmlVersion" &&
                                    elem.getAttribute("var")) ||
                                (child.nodeName === "wholeText" &&
                                    elem.getAttribute("var")) ||
                                (child.nodeName === "baseURI" &&
                                    elem.getAttribute("var"))
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}()${
                                        child.nodeName === "nodeValue" ||
                                            child.nodeName === "namespaceURI" ||
                                            child.nodeName === "localName" ||
                                            child.nodeName === "prefix" ||
                                            child.nodeName === "documentURI" ||
                                            child.nodeName === "baseURI" ||
                                            child.nodeName === "xmlEncoding" ||
                                            child.nodeName ===
                                                "inputEncoding" ||
                                            child.nodeName === "xmlVersion"
                                            ? ".unwrap()"
                                            : ""
                                    }.to_string();`;
                                }
                            } else if (
                                child.nodeName === "childNodes" ||
                                child.nodeName === "nodeType" ||
                                child.nodeName === "attributes" ||
                                child.nodeName === "hasChildNodes" ||
                                child.nodeName === "hasAttributes" ||
                                child.nodeName === "target" ||
                                child.nodeName === "specified" ||
                                child.nodeName ===
                                    "isElementContentWhitespace" ||
                                child.nodeName === "isId" ||
                                child.nodeName === "xmlStandalone"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}();`;
                                }
                            } else if (
                                child.nodeName === "textContent" &&
                                elem.getAttribute("var")
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}().unwrap();`;
                                }
                            } else if (
                                child.nodeName === "nodeValue" ||
                                child.nodeName === "value" ||
                                child.nodeName === "prefix" ||
                                child.nodeName === "documentURI" ||
                                child.nodeName === "baseURI" ||
                                child.nodeName === "textContent" ||
                                child.nodeName === "xmlVersion"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const value = elem.getAttribute("value");
                                if (obj && value) {
                                    buffer += `r#${snakeCase(obj)}.set_${
                                        snakeCase(child.nodeName)
                                    }(${
                                        value.startsWith('"')
                                            ? value
                                            : `r#${snakeCase(value)}`
                                    }).unwrap().to_string();`;
                                }
                            } else if (
                                child.nodeName === "appendChild" ||
                                child.nodeName === "removeChild"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const new_child =
                                    elem.getAttribute("newChild") ||
                                    elem.getAttribute("oldChild");
                                if (vr && obj && new_child) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(r#${
                                        snakeCase(new_child)
                                    }.into()).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "replaceChild"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const new_child = elem.getAttribute("newChild");
                                const old_child = elem.getAttribute("oldChild");
                                if (vr && obj && new_child && old_child) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(r#${
                                        snakeCase(new_child)
                                    }.into(), r#${
                                        snakeCase(old_child)
                                    }.into()).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "insertBefore"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const new_child = elem.getAttribute("newChild");
                                const old_child = elem.getAttribute("refChild");
                                if (vr && obj && new_child) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(r#${
                                        snakeCase(new_child)
                                    }.into(), ${
                                        old_child
                                            ? `Some(r#${
                                                snakeCase(old_child)
                                            }.into())`
                                            : `None`
                                    }).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "isEqualNode" ||
                                child.nodeName === "isSameNode"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const arg = elem.getAttribute("arg") ||
                                    elem.getAttribute("other");
                                if (vr && obj && arg) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(&r#${
                                        snakeCase(arg)
                                    }.into());`;
                                }
                            } else if (child.nodeName === "cloneNode") {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const deep = elem.getAttribute("deep");
                                if (vr && obj && deep) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        snakeCase(deep)
                                    });`;
                                }
                            } else if (child.nodeName === "importNode") {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const imported = elem.getAttribute(
                                    "importedNode",
                                );
                                const deep = elem.getAttribute("deep");
                                if (vr && obj && imported && deep) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(r#${
                                        snakeCase(imported)
                                    }.into(), ${snakeCase(deep)}).unwrap();`;
                                }
                            } else if (child.nodeName === "adoptNode") {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const source = elem.getAttribute(
                                    "source",
                                );
                                if (vr && obj && source) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(r#${
                                        snakeCase(source)
                                    }.into());`;
                                }
                            } else if (child.nodeName === "appendData") {
                                const arg = elem.getAttribute("arg");
                                const obj = elem.getAttribute("obj");
                                if (arg && obj) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${
                                        arg.startsWith('"')
                                            ? arg
                                            : `r#${snakeCase(arg)}`
                                    }).unwrap();`;
                                }
                            } else if (child.nodeName === "replaceData") {
                                const obj = elem.getAttribute("obj");
                                const offset = elem.getAttribute("offset");
                                const count = elem.getAttribute("count");
                                const arg = elem.getAttribute("arg");
                                if (arg && offset && count && obj) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${offset}, ${count}, ${
                                        arg.startsWith('"')
                                            ? arg
                                            : `r#${snakeCase(arg)}`
                                    }).unwrap();`;
                                }
                            } else if (child.nodeName === "insertData") {
                                const obj = elem.getAttribute("obj");
                                const offset = elem.getAttribute("offset");
                                const arg = elem.getAttribute("arg");
                                if (arg && offset && obj) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${offset}, ${
                                        arg.startsWith('"')
                                            ? arg
                                            : `r#${snakeCase(arg)}`
                                    }).unwrap();`;
                                }
                            } else if (child.nodeName === "deleteData") {
                                const obj = elem.getAttribute("obj");
                                const offset = elem.getAttribute("offset");
                                const count = elem.getAttribute("count");
                                if (offset && count && obj) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${offset}, ${count}).unwrap();`;
                                }
                            } else if (child.nodeName === "replaceWholeText") {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const content = elem.getAttribute("content");
                                if (vr && content && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        content.startsWith('"')
                                            ? content
                                            : `r#${snakeCase(content)}`
                                    }).unwrap().unwrap();`;
                                }
                            } else if (child.nodeName === "data") {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const iface = elem.getAttribute("interface");
                                if (vr && obj) {
                                    if (iface === "CharacterData") {
                                        buffer += `r#${snakeCase(vr)} = r#${
                                            snakeCase(obj)
                                        }.data().to_string();`;
                                    } else if (
                                        iface === "ProcessingInstruction"
                                    ) {
                                        buffer += `r#${snakeCase(vr)} = r#${
                                            snakeCase(obj)
                                        }.data().unwrap().to_string();`;
                                    } else {
                                        buffer += `\n// unimplemented: `;
                                    }
                                }
                            } else if (child.nodeName === "length") {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const iface = elem.getAttribute("interface");
                                if (vr && obj) {
                                    if (
                                        iface === "NodeList" ||
                                        iface === "DOMString" ||
                                        iface === "NamedNodeMap" ||
                                        iface === "CharacterData"
                                    ) {
                                        buffer += `r#${snakeCase(vr)} = r#${
                                            snakeCase(obj)
                                        }.len();`;
                                    } else {
                                        buffer += `\n// unimplemented: `;
                                    }
                                }
                            } else if (
                                child.nodeName === "normalize" ||
                                child.nodeName === "normalizeDocument"
                            ) {
                                const obj = elem.getAttribute("obj");
                                if (obj) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }();`;
                                }
                            } else if (child.nodeName === "increment") {
                                const vr = elem.getAttribute("var");
                                const value = elem.getAttribute("value");
                                if (vr) {
                                    buffer += `r#${snakeCase(vr)} += ${value};`;
                                }
                            } else if (child.nodeName === "decrement") {
                                const vr = elem.getAttribute("var");
                                const value = elem.getAttribute("value");
                                if (vr) {
                                    buffer += `r#${snakeCase(vr)} -= ${value};`;
                                }
                            } else if (child.nodeName === "getElementById") {
                                const obj = elem.getAttribute("obj");
                                const vr = elem.getAttribute("var");
                                const element_id = elem.getAttribute(
                                    "elementId",
                                );
                                if (vr && obj && element_id) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        element_id?.startsWith('"')
                                            ? element_id
                                            : `r#${snakeCase(element_id)}`
                                    }.as_ref()).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "getElementsByTagNameNS"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const local_name = elem.getAttribute(
                                    "localName",
                                );
                                if (vr && obj && local_name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri}.into())`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                }.into())`)
                                            : "None"
                                    }, ${
                                        local_name.startsWith('"')
                                            ? local_name
                                            : `r#${snakeCase(local_name)}`
                                    }.into()).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "hasAttributeNS"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const local_name = elem.getAttribute(
                                    "localName",
                                );
                                if (vr && obj && local_name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri})`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                }.as_ref())`)
                                            : "None"
                                    }, ${
                                        local_name.startsWith('"')
                                            ? local_name
                                            : `r#${
                                                snakeCase(local_name)
                                            }.as_ref()`
                                    }).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "getNamedItemNS"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const local_name = elem.getAttribute(
                                    "localName",
                                );
                                if (vr && obj && local_name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri})`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                })`)
                                            : "None"
                                    }, ${
                                        local_name.startsWith('"')
                                            ? local_name
                                            : `r#${snakeCase(local_name)}`
                                    }).unwrap().unwrap();`;
                                }
                            } else if (
                                child.nodeName === "getAttributeNodeNS"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const local_name = elem.getAttribute(
                                    "localName",
                                );
                                if (vr && obj && local_name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri})`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                })`)
                                            : "None"
                                    }, ${
                                        local_name.startsWith('"')
                                            ? local_name
                                            : `r#${snakeCase(local_name)}`
                                    }).unwrap().unwrap();`;
                                }
                            } else if (
                                child.nodeName === "getAttributeNS"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const local_name = elem.getAttribute(
                                    "localName",
                                );
                                if (vr && obj && local_name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri})`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                })`)
                                            : "None"
                                    }, ${
                                        local_name.startsWith('"')
                                            ? local_name
                                            : `r#${snakeCase(local_name)}`
                                    }).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "createAttributeNS" ||
                                child.nodeName === "createElementNS"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const qname = elem.getAttribute(
                                    "qualifiedName",
                                );
                                if (vr && obj && qname) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri}.as_ref())`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                }.as_ref())`)
                                            : "None"
                                    }, ${
                                        qname.startsWith('"')
                                            ? qname
                                            : `r#${snakeCase(qname)}`
                                    }.as_ref()).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "removeAttributeNS"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const local_name = elem.getAttribute(
                                    "localName",
                                );
                                if (obj && local_name) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri})`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                })`)
                                            : "None"
                                    }, ${
                                        local_name.startsWith('"')
                                            ? local_name
                                            : `r#${snakeCase(local_name)}`
                                    }).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "removeNamedItemNS"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const local_name = elem.getAttribute(
                                    "localName",
                                );
                                if (vr && obj && local_name) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri})`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                })`)
                                            : "None"
                                    }, ${
                                        local_name.startsWith('"')
                                            ? local_name
                                            : `r#${snakeCase(local_name)}`
                                    }).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "setAttributeNS"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const qname = elem.getAttribute(
                                    "qualifiedName",
                                );
                                const value = elem.getAttribute("value");
                                if (obj && qname && value) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri})`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                })`)
                                            : "None"
                                    }, ${
                                        qname.startsWith('"')
                                            ? qname
                                            : `r#${snakeCase(qname)}`
                                    }, ${
                                        value.startsWith('"')
                                            ? value
                                            : `r#${snakeCase(value)}`
                                    }).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "setIdAttributeNS"
                            ) {
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const local_name = elem.getAttribute(
                                    "localName",
                                );
                                const is_id = elem.getAttribute("isId");
                                if (obj && local_name && is_id) {
                                    buffer += `r#${snakeCase(obj)}.${
                                        snakeCase(child.nodeName)
                                    }(${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri})`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                })`)
                                            : "None"
                                    }, ${
                                        local_name.startsWith('"')
                                            ? local_name
                                            : `r#${snakeCase(local_name)}`
                                    }, ${is_id}).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "lookupPrefix"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                if (vr && obj && ns_uri) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${
                                        snakeCase(child.nodeName)
                                    }(${(ns_uri.startsWith('"')
                                        ? `${ns_uri}`
                                        : `r#${
                                            snakeCase(ns_uri)
                                        }.as_ref()`)}).unwrap().to_string();`;
                                }
                            } else if (
                                child.nodeName === "lookupNamespaceURI"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const prefix = elem.getAttribute("prefix");
                                if (vr && obj && prefix) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${
                                        snakeCase(child.nodeName)
                                    }(${(prefix.startsWith('"')
                                        ? prefix
                                        : `r#${
                                            snakeCase(prefix)
                                        }.as_ref()`)}).unwrap().to_string();`;
                                }
                            } else if (
                                child.nodeName === "isDefaultNamespace"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                if (vr && obj && ns_uri) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${
                                        snakeCase(child.nodeName)
                                    }(${(ns_uri.startsWith('"')
                                        ? `${ns_uri}`
                                        : `r#${
                                            snakeCase(ns_uri)
                                        }.as_ref()`)});`;
                                }
                            } else if (
                                child.nodeName === "renameNode"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const n = elem.getAttribute("n");
                                const ns_uri = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const qname = elem.getAttribute(
                                    "qualifiedName",
                                );
                                if (vr && obj && n && qname) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(r#${
                                        snakeCase(n)
                                    }.into(), ${
                                        ns_uri
                                            ? (ns_uri.startsWith('"')
                                                ? `Some(${ns_uri})`
                                                : `Some(r#${
                                                    snakeCase(ns_uri)
                                                })`)
                                            : "None"
                                    }, ${
                                        qname.startsWith('"')
                                            ? qname
                                            : `r#${snakeCase(qname)}`
                                    }).unwrap();`;
                                }
                            } else if (
                                child.nodeName === "compareDocumentPosition"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const other = elem.getAttribute("other");
                                if (vr && obj && other) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}(
                                        &r#${snakeCase(other)}.into());`;
                                }
                            } else if (
                                child.nodeName === "notations" ||
                                child.nodeName === "entities" ||
                                child.nodeName === "implementation"
                            ) {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                if (vr && obj) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${snakeCase(child.nodeName)}();`;
                                }
                            } else if (child.nodeName === "createDocument") {
                                const vr = elem.getAttribute("var");
                                const obj = elem.getAttribute("obj");
                                const namespaceURI = elem.getAttribute(
                                    "namespaceURI",
                                );
                                const qualifiedName = elem.getAttribute(
                                    "qualifiedName",
                                );
                                const doctype = elem.getAttribute("doctype");
                                if (
                                    vr && obj && namespaceURI &&
                                    qualifiedName && doctype
                                ) {
                                    buffer += `r#${snakeCase(vr)} = r#${
                                        snakeCase(obj)
                                    }.${
                                        snakeCase(child.nodeName)
                                    }().unwrap(Some(${
                                        namespaceURI.startsWith('"')
                                            ? namespaceURI
                                            : `r#${snakeCase(namespaceURI)}`
                                    }), Some(${
                                        qualifiedName.startsWith('"')
                                            ? qualifiedName
                                            : `r#${snakeCase(qualifiedName)}`
                                    }), Some(r#${snakeCase(doctype)}));`;
                                }
                            } else {
                                buffer += `\n// unimplemented: `;
                            }
                            buffer += `// ${`${child}`.replaceAll("\n", "")}\n`;
                        }
                        child = child.nextSibling;
                    }
                }

                buffer += "}";
            }
        }
        buffer += "}";
        Deno.chdir("../");
    }

    buffer += "}";
    Deno.chdir("../");
}
Deno.chdir("../../");
buffer += "}";
buffer = buffer.replaceAll(
    ' xmlns="http://www.w3.org/2001/DOM-Test-Suite/Level-1"',
    "",
);
buffer = buffer.replaceAll(
    ' xmlns="http://www.w3.org/2001/DOM-Test-Suite/Level-2"',
    "",
);
buffer = buffer.replaceAll(
    ' xmlns="http://www.w3.org/2001/DOM-Test-Suite/Level-3"',
    "",
);

Deno.writeTextFileSync("./__dom_test_suite.rs", buffer, {
    create: true,
});
