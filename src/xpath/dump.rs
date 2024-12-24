use std::{ffi::CStr, io::Write};

#[cfg(feature = "libxml_xptr_locs")]
use crate::{libxml::xpointer::XmlLocationSetPtr, tree::XmlNodePtr};
use crate::{
    libxml::{
        debug_xml::{xml_debug_dump_attr, xml_debug_dump_one_node},
        xmlstring::XmlChar,
    },
    tree::{NodeCommon, XmlElementType},
    xpath::{
        xml_xpath_is_inf, xml_xpath_is_nan, XmlXPathAxisVal, XmlXPathObjectType, XmlXPathOp,
        XmlXPathTestVal, XmlXPathTypeVal,
    },
};

use super::{XmlNodeSet, XmlXPathCompExprPtr, XmlXPathObjectPtr, XmlXPathStepOpPtr};

unsafe fn xml_xpath_debug_dump_node<'a>(
    output: &mut (impl Write + 'a),
    cur: Option<&impl NodeCommon>,
    depth: i32,
) {
    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    let Some(cur) = cur else {
        write!(output, "{}", shift);
        writeln!(output, "Node is NULL !");
        return;
    };

    if cur.element_type() == XmlElementType::XmlDocumentNode
        || cur.element_type() == XmlElementType::XmlHTMLDocumentNode
    {
        write!(output, "{}", shift);
        writeln!(output, " /");
    } else if cur.element_type() == XmlElementType::XmlAttributeNode {
        xml_debug_dump_attr(output, cur.as_attribute_node().map(|n| n.as_ref()), depth);
    } else {
        xml_debug_dump_one_node(output, Some(cur), depth);
    }
}

unsafe fn xml_xpath_debug_dump_node_list<'a>(
    output: &mut (impl Write + 'a),
    cur: Option<&impl NodeCommon>,
    depth: i32,
) {
    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    let Some(cur) = cur else {
        write!(output, "{}", shift);
        writeln!(output, "Node is NULL !");
        return;
    };
    xml_debug_dump_one_node(output, Some(cur), depth);
    let mut cur = cur as &dyn NodeCommon;
    while let Some(next) = cur.next() {
        xml_debug_dump_one_node(output, Some(&*next.as_ptr()), depth);
        cur = &*next.as_ptr() as &dyn NodeCommon;
    }
}

unsafe fn xml_xpath_debug_dump_node_set<'a>(
    output: &mut (impl Write + 'a),
    cur: Option<&XmlNodeSet>,
    depth: i32,
) {
    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    let Some(cur) = cur else {
        write!(output, "{}", shift);
        writeln!(output, "NodeSet is NULL !");
        return;
    };

    writeln!(output, "Set contains {} nodes:", cur.node_tab.len());
    for (i, &node) in cur.node_tab.iter().enumerate() {
        write!(output, "{}", shift);
        write!(output, "{}", i + 1);
        xml_xpath_debug_dump_node(output, (!node.is_null()).then(|| &*node), depth + 1);
    }
}

unsafe fn xml_xpath_debug_dump_value_tree<'a>(
    output: &mut (impl Write + 'a),
    cur: Option<&XmlNodeSet>,
    depth: i32,
) {
    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    let Some(cur) = cur.filter(|c| !c.is_empty() && !c.get(0).is_null()) else {
        write!(output, "{}", shift);
        writeln!(output, "Value Tree is NULL !");
        return;
    };

    write!(output, "{}", shift);
    write!(output, "{}", depth.clamp(0, 25) + 1);
    xml_xpath_debug_dump_node_list(
        output,
        (*cur.node_tab[0]).children().map(|c| &*c.as_ptr()),
        depth + 1,
    );
}

#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xpath_debug_dump_location_set<'a>(
    output: &mut (impl Write + 'a),
    cur: XmlLocationSetPtr,
    depth: i32,
) {
    let shift = "  ".repeat(depth.clamp(0, 25) as usize);
    if cur.is_null() {
        write!(output, "{}", shift).ok();
        writeln!(output, "LocationSet is NULL !").ok();
        return;
    }

    for i in 0..(*cur).loc_nr {
        write!(output, "{}", shift);
        write!(output, "{} : ", i + 1);
        xml_xpath_debug_dump_object(output, *(*cur).loc_tab.add(i as usize), depth + 1);
    }
}

/// Dump the content of the object for debugging purposes
#[doc(alias = "xmlXPathDebugDumpObject")]
pub unsafe fn xml_xpath_debug_dump_object<'a>(
    output: &mut (impl Write + 'a),
    cur: XmlXPathObjectPtr,
    depth: i32,
) {
    use crate::libxml::debug_xml::xml_debug_dump_string;

    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    write!(output, "{}", shift);

    if cur.is_null() {
        writeln!(output, "Object is empty (NULL)");
        return;
    }
    match (*cur).typ {
        XmlXPathObjectType::XPathUndefined => {
            writeln!(output, "Object is uninitialized");
        }
        XmlXPathObjectType::XPathNodeset => {
            writeln!(output, "Object is a Node Set :");
            xml_xpath_debug_dump_node_set(output, (*cur).nodesetval.as_deref(), depth);
        }
        XmlXPathObjectType::XPathXSLTTree => {
            writeln!(output, "Object is an XSLT value tree :");
            xml_xpath_debug_dump_value_tree(output, (*cur).nodesetval.as_deref(), depth);
        }
        XmlXPathObjectType::XPathBoolean => {
            write!(output, "Object is a Boolean : ");
            if (*cur).boolval {
                writeln!(output, "true");
            } else {
                writeln!(output, "false");
            }
        }
        XmlXPathObjectType::XPathNumber => {
            match xml_xpath_is_inf((*cur).floatval) {
                1 => {
                    writeln!(output, "Object is a number : Infinity");
                }
                -1 => {
                    writeln!(output, "Object is a number : -Infinity");
                }
                _ => {
                    if xml_xpath_is_nan((*cur).floatval) {
                        writeln!(output, "Object is a number : NaN");
                    } else if (*cur).floatval == 0.0 {
                        /* Omit sign for negative zero. */
                        writeln!(output, "Object is a number : 0");
                    } else {
                        writeln!(output, "Object is a number : {}", (*cur).floatval);
                    }
                }
            }
        }
        XmlXPathObjectType::XPathString => {
            write!(output, "Object is a string : ");
            let strval = (*cur).stringval.as_deref();
            xml_debug_dump_string(Some(output), strval);
            writeln!(output);
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathPoint => {
            write!(output, "Object is a point : index {} in node", (*cur).index,);
            let node = (*cur).user as XmlNodePtr;
            xml_xpath_debug_dump_node(output, (!node.is_null()).then(|| &*node), depth + 1);
            writeln!(output);
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathRange => {
            if (*cur).user2.is_null()
                || ((*cur).user2 == (*cur).user && (*cur).index == (*cur).index2)
            {
                writeln!(output, "Object is a collapsed range :");
                write!(output, "{}", shift);
                if (*cur).index >= 0 {
                    write!(output, "index {} in ", (*cur).index);
                }
                writeln!(output, "node");
                let node = (*cur).user as XmlNodePtr;
                xml_xpath_debug_dump_node(output, (!node.is_null()).then(|| &*node), depth + 1);
            } else {
                writeln!(output, "Object is a range :");
                write!(output, "{}", shift);
                write!(output, "From ");
                if (*cur).index >= 0 {
                    write!(output, "index {} in ", (*cur).index);
                }
                writeln!(output, "node");
                let node = (*cur).user as XmlNodePtr;
                xml_xpath_debug_dump_node(output, (!node.is_null()).then(|| &*node), depth + 1);
                write!(output, "{}", shift);
                write!(output, "To ");
                if (*cur).index2 >= 0 {
                    write!(output, "index {} in ", (*cur).index2);
                }
                writeln!(output, "node");
                let node = (*cur).user2 as XmlNodePtr;
                xml_xpath_debug_dump_node(output, (!node.is_null()).then(|| &*node), depth + 1);
                writeln!(output);
            }
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathObjectType::XPathLocationset => {
            writeln!(output, "Object is a Location Set:");
            xml_xpath_debug_dump_location_set(output, (*cur).user as XmlLocationSetPtr, depth);
        }
        XmlXPathObjectType::XPathUsers => {
            writeln!(output, "Object is user defined");
        }
    }
}

unsafe fn xml_xpath_debug_dump_step_op<'a>(
    output: &mut (impl Write + 'a),
    comp: XmlXPathCompExprPtr,
    op: XmlXPathStepOpPtr,
    depth: i32,
) {
    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    write!(output, "{}", shift);
    if op.is_null() {
        writeln!(output, "Step is NULL");
        return;
    }
    match (*op).op {
        XmlXPathOp::XpathOpEnd => {
            write!(output, "END");
        }
        XmlXPathOp::XpathOpAnd => {
            write!(output, "AND");
        }
        XmlXPathOp::XpathOpOr => {
            write!(output, "OR");
        }
        XmlXPathOp::XpathOpEqual => {
            if (*op).value != 0 {
                write!(output, "EQUAL =");
            } else {
                write!(output, "EQUAL !=");
            }
        }
        XmlXPathOp::XpathOpCmp => {
            if (*op).value != 0 {
                write!(output, "CMP <");
            } else {
                write!(output, "CMP >");
            }
            if (*op).value2 == 0 {
                write!(output, "=");
            }
        }
        XmlXPathOp::XpathOpPlus => {
            if (*op).value == 0 {
                write!(output, "PLUS -");
            } else if (*op).value == 1 {
                write!(output, "PLUS +");
            } else if (*op).value == 2 {
                write!(output, "PLUS unary -");
            } else if (*op).value == 3 {
                write!(output, "PLUS unary - -");
            }
        }
        XmlXPathOp::XpathOpMult => {
            if (*op).value == 0 {
                write!(output, "MULT *");
            } else if (*op).value == 1 {
                write!(output, "MULT div");
            } else {
                write!(output, "MULT mod");
            }
        }
        XmlXPathOp::XpathOpUnion => {
            write!(output, "UNION");
        }
        XmlXPathOp::XpathOpRoot => {
            write!(output, "ROOT");
        }
        XmlXPathOp::XpathOpNode => {
            write!(output, "NODE");
        }
        XmlXPathOp::XpathOpSort => {
            write!(output, "SORT");
        }
        XmlXPathOp::XpathOpCollect => {
            let prefix: *const XmlChar = (*op).value4 as _;
            let name: *const XmlChar = (*op).value5 as _;

            write!(output, "COLLECT ");
            match XmlXPathAxisVal::try_from((*op).value) {
                Ok(XmlXPathAxisVal::AxisAncestor) => {
                    write!(output, " 'ancestors' ");
                }
                Ok(XmlXPathAxisVal::AxisAncestorOrSelf) => {
                    write!(output, " 'ancestors-or-self' ");
                }
                Ok(XmlXPathAxisVal::AxisAttribute) => {
                    write!(output, " 'attributes' ");
                }
                Ok(XmlXPathAxisVal::AxisChild) => {
                    write!(output, " 'child' ");
                }
                Ok(XmlXPathAxisVal::AxisDescendant) => {
                    write!(output, " 'descendant' ");
                }
                Ok(XmlXPathAxisVal::AxisDescendantOrSelf) => {
                    write!(output, " 'descendant-or-self' ");
                }
                Ok(XmlXPathAxisVal::AxisFollowing) => {
                    write!(output, " 'following' ");
                }
                Ok(XmlXPathAxisVal::AxisFollowingSibling) => {
                    write!(output, " 'following-siblings' ");
                }
                Ok(XmlXPathAxisVal::AxisNamespace) => {
                    write!(output, " 'namespace' ");
                }
                Ok(XmlXPathAxisVal::AxisParent) => {
                    write!(output, " 'parent' ");
                }
                Ok(XmlXPathAxisVal::AxisPreceding) => {
                    write!(output, " 'preceding' ");
                }
                Ok(XmlXPathAxisVal::AxisPrecedingSibling) => {
                    write!(output, " 'preceding-sibling' ");
                }
                Ok(XmlXPathAxisVal::AxisSelf) => {
                    write!(output, " 'self' ");
                }
                _ => unreachable!(),
            }
            match XmlXPathTestVal::try_from((*op).value2) {
                Ok(XmlXPathTestVal::NodeTestNone) => {
                    write!(output, "'none' ");
                }
                Ok(XmlXPathTestVal::NodeTestType) => {
                    write!(output, "'type' ");
                }
                Ok(XmlXPathTestVal::NodeTestPI) => {
                    write!(output, "'PI' ");
                }
                Ok(XmlXPathTestVal::NodeTestAll) => {
                    write!(output, "'all' ");
                }
                Ok(XmlXPathTestVal::NodeTestNs) => {
                    write!(output, "'namespace' ");
                }
                Ok(XmlXPathTestVal::NodeTestName) => {
                    write!(output, "'name' ");
                }
                _ => unreachable!(),
            }
            match XmlXPathTypeVal::try_from((*op).value3) {
                Ok(XmlXPathTypeVal::NodeTypeNode) => {
                    write!(output, "'node' ");
                }
                Ok(XmlXPathTypeVal::NodeTypeComment) => {
                    write!(output, "'comment' ");
                }
                Ok(XmlXPathTypeVal::NodeTypeText) => {
                    write!(output, "'text' ");
                }
                Ok(XmlXPathTypeVal::NodeTypePI) => {
                    write!(output, "'PI' ");
                }
                _ => unreachable!(),
            }
            if !prefix.is_null() {
                let prefix = CStr::from_ptr(prefix as *const i8).to_string_lossy();
                write!(output, "{}:", prefix);
            }
            if !name.is_null() {
                let name = CStr::from_ptr(name as *const i8).to_string_lossy();
                write!(output, "{}", name);
            }
        }
        XmlXPathOp::XpathOpValue => {
            let object: XmlXPathObjectPtr = (*op).value4 as XmlXPathObjectPtr;

            write!(output, "ELEM ");
            xml_xpath_debug_dump_object(output, object, 0);
            // goto finish;
            if (*op).ch1 >= 0 {
                xml_xpath_debug_dump_step_op(
                    output,
                    comp,
                    (*comp).steps.add((*op).ch1 as usize),
                    depth + 1,
                );
            }
            if (*op).ch2 >= 0 {
                xml_xpath_debug_dump_step_op(
                    output,
                    comp,
                    (*comp).steps.add((*op).ch2 as usize),
                    depth + 1,
                );
            }
            return;
        }
        XmlXPathOp::XpathOpVariable => {
            let prefix: *const XmlChar = (*op).value5 as _;
            let name: *const XmlChar = (*op).value4 as _;
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();

            if !prefix.is_null() {
                let prefix = CStr::from_ptr(prefix as *const i8).to_string_lossy();
                write!(output, "VARIABLE {prefix}:{name}");
            } else {
                write!(output, "VARIABLE {name}");
            }
        }
        XmlXPathOp::XpathOpFunction => {
            let nbargs: i32 = (*op).value;
            let prefix: *const XmlChar = (*op).value5 as _;
            let name: *const XmlChar = (*op).value4 as _;
            let name = CStr::from_ptr(name as *const i8).to_string_lossy();

            if !prefix.is_null() {
                let prefix = CStr::from_ptr(prefix as *const i8).to_string_lossy();
                write!(output, "FUNCTION {prefix}:{name}({nbargs} args)");
            } else {
                write!(output, "FUNCTION {name}({nbargs} args)");
            }
        }
        XmlXPathOp::XpathOpArg => {
            write!(output, "ARG");
        }
        XmlXPathOp::XpathOpPredicate => {
            write!(output, "PREDICATE");
        }
        XmlXPathOp::XpathOpFilter => {
            write!(output, "FILTER");
        }
        #[cfg(feature = "libxml_xptr_locs")]
        XmlXPathOp::XpathOpRangeto => {
            write!(output, "RANGETO");
        } // _ => {
          //     write!(output, "UNKNOWN %d\n", (*op).op);
          //     return;
          // }
    }
    writeln!(output);
    // finish:
    if (*op).ch1 >= 0 {
        xml_xpath_debug_dump_step_op(
            output,
            comp,
            (*comp).steps.add((*op).ch1 as usize),
            depth + 1,
        );
    }
    if (*op).ch2 >= 0 {
        xml_xpath_debug_dump_step_op(
            output,
            comp,
            (*comp).steps.add((*op).ch2 as usize),
            depth + 1,
        );
    }
}

/// Dumps the tree of the compiled XPath expression.
#[doc(alias = "xmlXPathDebugDumpCompExpr")]
pub unsafe fn xml_xpath_debug_dump_comp_expr<'a>(
    output: &mut (impl Write + 'a),
    comp: XmlXPathCompExprPtr,
    depth: i32,
) {
    if comp.is_null() {
        return;
    }

    let shift = "  ".repeat(depth.clamp(0, 25) as usize);

    write!(output, "{}", shift);

    if !(*comp).stream.is_null() {
        writeln!(output, "Streaming Expression");
    } else {
        writeln!(output, "Compiled Expression : {} elements", (*comp).nb_step,);
        xml_xpath_debug_dump_step_op(
            output,
            comp,
            (*comp).steps.add((*comp).last as usize),
            depth + 1,
        );
    }
}
