use std::{io::Write, ptr::null_mut};

#[cfg(feature = "libxml_xptr_locs")]
use crate::libxml::xpointer::XmlLocationSetPtr;
use crate::{
    debug_xml::{xml_debug_dump_attr, xml_debug_dump_one_node, xml_debug_dump_string},
    tree::{XmlAttrPtr, XmlElementType, XmlGenericNodePtr},
    xpath::{
        XmlXPathAxisVal, XmlXPathObjectType, XmlXPathOp, XmlXPathTestVal, XmlXPathTypeVal,
        xml_xpath_is_inf, xml_xpath_is_nan,
    },
};

use super::{XmlNodeSet, XmlXPathCompExpr, XmlXPathObjectPtr, compile::XmlXPathStepOpPtr};

unsafe fn xml_xpath_debug_dump_node<'a>(
    output: &mut (impl Write + 'a),
    cur: Option<XmlGenericNodePtr>,
    depth: i32,
) {
    unsafe {
        let shift = "  ".repeat(depth.clamp(0, 25) as usize);

        let Some(cur) = cur else {
            write!(output, "{}", shift).ok();
            writeln!(output, "Node is NULL !").ok();
            return;
        };

        if cur.element_type() == XmlElementType::XmlDocumentNode
            || cur.element_type() == XmlElementType::XmlHTMLDocumentNode
        {
            write!(output, "{}", shift).ok();
            writeln!(output, " /").ok();
        } else if cur.element_type() == XmlElementType::XmlAttributeNode {
            xml_debug_dump_attr(output, XmlAttrPtr::try_from(cur).ok(), depth);
        } else {
            xml_debug_dump_one_node(output, Some(cur), depth);
        }
    }
}

unsafe fn xml_xpath_debug_dump_node_list<'a>(
    output: &mut (impl Write + 'a),
    cur: Option<XmlGenericNodePtr>,
    depth: i32,
) {
    unsafe {
        let shift = "  ".repeat(depth.clamp(0, 25) as usize);

        let Some(mut cur) = cur else {
            write!(output, "{}", shift).ok();
            writeln!(output, "Node is NULL !").ok();
            return;
        };
        xml_debug_dump_one_node(output, Some(cur), depth);
        while let Some(next) = cur.next() {
            xml_debug_dump_one_node(output, Some(next), depth);
            cur = next;
        }
    }
}

unsafe fn xml_xpath_debug_dump_node_set<'a>(
    output: &mut (impl Write + 'a),
    cur: Option<&XmlNodeSet>,
    depth: i32,
) {
    unsafe {
        let shift = "  ".repeat(depth.clamp(0, 25) as usize);

        let Some(cur) = cur else {
            write!(output, "{}", shift).ok();
            writeln!(output, "NodeSet is NULL !").ok();
            return;
        };

        writeln!(output, "Set contains {} nodes:", cur.node_tab.len()).ok();
        for (i, &node) in cur.node_tab.iter().enumerate() {
            write!(output, "{}", shift).ok();
            write!(output, "{}", i + 1).ok();
            xml_xpath_debug_dump_node(output, Some(node), depth + 1);
        }
    }
}

unsafe fn xml_xpath_debug_dump_value_tree<'a>(
    output: &mut (impl Write + 'a),
    cur: Option<&XmlNodeSet>,
    depth: i32,
) {
    unsafe {
        let shift = "  ".repeat(depth.clamp(0, 25) as usize);

        let Some(cur) = cur.filter(|c| !c.is_empty() && c.get(0).is_some()) else {
            write!(output, "{}", shift).ok();
            writeln!(output, "Value Tree is NULL !").ok();
            return;
        };

        write!(output, "{}", shift).ok();
        write!(output, "{}", depth.clamp(0, 25) + 1).ok();
        xml_xpath_debug_dump_node_list(output, (*cur.node_tab[0]).children(), depth + 1);
    }
}

#[cfg(feature = "libxml_xptr_locs")]
unsafe fn xml_xpath_debug_dump_location_set<'a>(
    output: &mut (impl Write + 'a),
    cur: XmlLocationSetPtr,
    depth: i32,
) {
    unsafe {
        let shift = "  ".repeat(depth.clamp(0, 25) as usize);
        if cur.is_null() {
            write!(output, "{}", shift).ok();
            writeln!(output, "LocationSet is NULL !").ok();
            return;
        }

        for (i, &loc) in (*cur).loc_tab.iter().enumerate() {
            write!(output, "{}", shift).ok();
            write!(output, "{} : ", i + 1).ok();
            xml_xpath_debug_dump_object(output, loc, depth + 1);
        }
    }
}

/// Dump the content of the object for debugging purposes
#[doc(alias = "xmlXPathDebugDumpObject")]
pub unsafe fn xml_xpath_debug_dump_object<'a>(
    output: &mut (impl Write + 'a),
    cur: XmlXPathObjectPtr,
    depth: i32,
) {
    unsafe {
        let shift = "  ".repeat(depth.clamp(0, 25) as usize);

        write!(output, "{}", shift).ok();

        if cur.is_null() {
            writeln!(output, "Object is empty (NULL)").ok();
            return;
        }
        match (*cur).typ {
            XmlXPathObjectType::XPathUndefined => {
                writeln!(output, "Object is uninitialized").ok();
            }
            XmlXPathObjectType::XPathNodeset => {
                writeln!(output, "Object is a Node Set :").ok();
                xml_xpath_debug_dump_node_set(output, (*cur).nodesetval.as_deref(), depth);
            }
            XmlXPathObjectType::XPathXSLTTree => {
                writeln!(output, "Object is an XSLT value tree :").ok();
                xml_xpath_debug_dump_value_tree(output, (*cur).nodesetval.as_deref(), depth);
            }
            XmlXPathObjectType::XPathBoolean => {
                write!(output, "Object is a Boolean : ").ok();
                if (*cur).boolval {
                    writeln!(output, "true").ok();
                } else {
                    writeln!(output, "false").ok();
                }
            }
            XmlXPathObjectType::XPathNumber => {
                match xml_xpath_is_inf((*cur).floatval) {
                    1 => {
                        writeln!(output, "Object is a number : Infinity").ok();
                    }
                    -1 => {
                        writeln!(output, "Object is a number : -Infinity").ok();
                    }
                    _ => {
                        if xml_xpath_is_nan((*cur).floatval) {
                            writeln!(output, "Object is a number : NaN").ok();
                        } else if (*cur).floatval == 0.0 {
                            /* Omit sign for negative zero. */
                            writeln!(output, "Object is a number : 0").ok();
                        } else {
                            writeln!(output, "Object is a number : {}", (*cur).floatval).ok();
                        }
                    }
                }
            }
            XmlXPathObjectType::XPathString => {
                write!(output, "Object is a string : ").ok();
                let strval = (*cur).stringval.as_deref();
                xml_debug_dump_string(Some(output), strval);
                writeln!(output).ok();
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathObjectType::XPathPoint => {
                write!(output, "Object is a point : index {} in node", (*cur).index,).ok();
                let node = (*cur)
                    .user
                    .as_ref()
                    .and_then(|user| user.as_node())
                    .copied();
                xml_xpath_debug_dump_node(output, node, depth + 1);
                writeln!(output).ok();
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathObjectType::XPathRange => {
                if (*cur).user2.as_ref().is_none_or(|user| {
                    Some(user) == (*cur).user.as_ref() && (*cur).index == (*cur).index2
                }) {
                    writeln!(output, "Object is a collapsed range :").ok();
                    write!(output, "{}", shift).ok();
                    if (*cur).index >= 0 {
                        write!(output, "index {} in ", (*cur).index).ok();
                    }
                    writeln!(output, "node").ok();
                    let node = (*cur)
                        .user
                        .as_ref()
                        .and_then(|user| user.as_node())
                        .copied();
                    xml_xpath_debug_dump_node(output, node, depth + 1);
                } else {
                    writeln!(output, "Object is a range :").ok();
                    write!(output, "{}", shift).ok();
                    write!(output, "From ").ok();
                    if (*cur).index >= 0 {
                        write!(output, "index {} in ", (*cur).index).ok();
                    }
                    writeln!(output, "node").ok();
                    let node = (*cur)
                        .user
                        .as_ref()
                        .and_then(|user| user.as_node())
                        .copied();
                    xml_xpath_debug_dump_node(output, node, depth + 1);
                    write!(output, "{}", shift).ok();
                    write!(output, "To ").ok();
                    if (*cur).index2 >= 0 {
                        write!(output, "index {} in ", (*cur).index2).ok();
                    }
                    writeln!(output, "node").ok();
                    let node = (*cur)
                        .user2
                        .as_ref()
                        .and_then(|user| user.as_node())
                        .copied();
                    xml_xpath_debug_dump_node(output, node, depth + 1);
                    writeln!(output).ok();
                }
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathObjectType::XPathLocationset => {
                writeln!(output, "Object is a Location Set:").ok();
                let loc = (*cur)
                    .user
                    .as_ref()
                    .and_then(|user| user.as_location_set())
                    .copied()
                    .unwrap();
                xml_xpath_debug_dump_location_set(output, loc, depth);
            }
            XmlXPathObjectType::XPathUsers => {
                writeln!(output, "Object is user defined").ok();
            }
        }
    }
}

unsafe fn xml_xpath_debug_dump_step_op<'a>(
    output: &mut (impl Write + 'a),
    comp: &XmlXPathCompExpr,
    op: XmlXPathStepOpPtr,
    depth: i32,
) {
    unsafe {
        let shift = "  ".repeat(depth.clamp(0, 25) as usize);

        write!(output, "{}", shift).ok();
        if op.is_null() {
            writeln!(output, "Step is NULL").ok();
            return;
        }
        match (*op).op {
            XmlXPathOp::XPathOpEnd => {
                write!(output, "END").ok();
            }
            XmlXPathOp::XPathOpAnd => {
                write!(output, "AND").ok();
            }
            XmlXPathOp::XPathOpOr => {
                write!(output, "OR").ok();
            }
            XmlXPathOp::XPathOpEqual => {
                if (*op).value != 0 {
                    write!(output, "EQUAL =").ok();
                } else {
                    write!(output, "EQUAL !=").ok();
                }
            }
            XmlXPathOp::XPathOpCmp => {
                if (*op).value != 0 {
                    write!(output, "CMP <").ok();
                } else {
                    write!(output, "CMP >").ok();
                }
                if (*op).value2 == 0 {
                    write!(output, "=").ok();
                }
            }
            XmlXPathOp::XPathOpPlus => {
                if (*op).value == 0 {
                    write!(output, "PLUS -").ok();
                } else if (*op).value == 1 {
                    write!(output, "PLUS +").ok();
                } else if (*op).value == 2 {
                    write!(output, "PLUS unary -").ok();
                } else if (*op).value == 3 {
                    write!(output, "PLUS unary - -").ok();
                }
            }
            XmlXPathOp::XPathOpMult => {
                if (*op).value == 0 {
                    write!(output, "MULT *").ok();
                } else if (*op).value == 1 {
                    write!(output, "MULT div").ok();
                } else {
                    write!(output, "MULT mod").ok();
                }
            }
            XmlXPathOp::XPathOpUnion => {
                write!(output, "UNION").ok();
            }
            XmlXPathOp::XPathOpRoot => {
                write!(output, "ROOT").ok();
            }
            XmlXPathOp::XPathOpNode => {
                write!(output, "NODE").ok();
            }
            XmlXPathOp::XPathOpSort => {
                write!(output, "SORT").ok();
            }
            XmlXPathOp::XPathOpCollect => {
                let prefix = (*op).value4.as_ref().and_then(|val| val.as_str());
                let name = (*op).value5.as_ref().and_then(|val| val.as_str());

                write!(output, "COLLECT ").ok();
                match XmlXPathAxisVal::try_from((*op).value) {
                    Ok(XmlXPathAxisVal::AxisAncestor) => {
                        write!(output, " 'ancestors' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisAncestorOrSelf) => {
                        write!(output, " 'ancestors-or-self' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisAttribute) => {
                        write!(output, " 'attributes' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisChild) => {
                        write!(output, " 'child' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisDescendant) => {
                        write!(output, " 'descendant' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisDescendantOrSelf) => {
                        write!(output, " 'descendant-or-self' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisFollowing) => {
                        write!(output, " 'following' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisFollowingSibling) => {
                        write!(output, " 'following-siblings' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisNamespace) => {
                        write!(output, " 'namespace' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisParent) => {
                        write!(output, " 'parent' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisPreceding) => {
                        write!(output, " 'preceding' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisPrecedingSibling) => {
                        write!(output, " 'preceding-sibling' ").ok();
                    }
                    Ok(XmlXPathAxisVal::AxisSelf) => {
                        write!(output, " 'self' ").ok();
                    }
                    _ => unreachable!(),
                }
                match XmlXPathTestVal::try_from((*op).value2) {
                    Ok(XmlXPathTestVal::NodeTestNone) => {
                        write!(output, "'none' ").ok();
                    }
                    Ok(XmlXPathTestVal::NodeTestType) => {
                        write!(output, "'type' ").ok();
                    }
                    Ok(XmlXPathTestVal::NodeTestPI) => {
                        write!(output, "'PI' ").ok();
                    }
                    Ok(XmlXPathTestVal::NodeTestAll) => {
                        write!(output, "'all' ").ok();
                    }
                    Ok(XmlXPathTestVal::NodeTestNs) => {
                        write!(output, "'namespace' ").ok();
                    }
                    Ok(XmlXPathTestVal::NodeTestName) => {
                        write!(output, "'name' ").ok();
                    }
                    _ => unreachable!(),
                }
                match XmlXPathTypeVal::try_from((*op).value3) {
                    Ok(XmlXPathTypeVal::NodeTypeNode) => {
                        write!(output, "'node' ").ok();
                    }
                    Ok(XmlXPathTypeVal::NodeTypeComment) => {
                        write!(output, "'comment' ").ok();
                    }
                    Ok(XmlXPathTypeVal::NodeTypeText) => {
                        write!(output, "'text' ").ok();
                    }
                    Ok(XmlXPathTypeVal::NodeTypePI) => {
                        write!(output, "'PI' ").ok();
                    }
                    _ => unreachable!(),
                }
                if let Some(prefix) = prefix {
                    write!(output, "{}:", prefix).ok();
                }
                if let Some(name) = name {
                    write!(output, "{}", name).ok();
                }
            }
            XmlXPathOp::XPathOpValue => {
                let object = (*op)
                    .value4
                    .as_ref()
                    .and_then(|val| val.as_object())
                    .map_or(null_mut(), |obj| *obj);

                write!(output, "ELEM ").ok();
                xml_xpath_debug_dump_object(output, object, 0);
                // goto finish;
                if (*op).ch1 >= 0 {
                    xml_xpath_debug_dump_step_op(
                        output,
                        comp,
                        &raw const comp.steps[(*op).ch1 as usize] as _,
                        depth + 1,
                    );
                }
                if (*op).ch2 >= 0 {
                    xml_xpath_debug_dump_step_op(
                        output,
                        comp,
                        &raw const comp.steps[(*op).ch2 as usize] as _,
                        depth + 1,
                    );
                }
                return;
            }
            XmlXPathOp::XPathOpVariable => {
                let prefix = (*op).value5.as_ref().and_then(|val| val.as_str());
                let name = (*op).value4.as_ref().and_then(|val| val.as_str()).unwrap();

                if let Some(prefix) = prefix {
                    write!(output, "VARIABLE {prefix}:{name}").ok();
                } else {
                    write!(output, "VARIABLE {name}").ok();
                }
            }
            XmlXPathOp::XPathOpFunction => {
                let nbargs: i32 = (*op).value;
                let prefix = (*op).value5.as_ref().and_then(|val| val.as_str());
                let name = (*op).value4.as_ref().and_then(|val| val.as_str()).unwrap();

                if let Some(prefix) = prefix {
                    write!(output, "FUNCTION {prefix}:{name}({nbargs} args)").ok();
                } else {
                    write!(output, "FUNCTION {name}({nbargs} args)").ok();
                }
            }
            XmlXPathOp::XPathOpArg => {
                write!(output, "ARG").ok();
            }
            XmlXPathOp::XPathOpPredicate => {
                write!(output, "PREDICATE").ok();
            }
            XmlXPathOp::XPathOpFilter => {
                write!(output, "FILTER").ok();
            }
            #[cfg(feature = "libxml_xptr_locs")]
            XmlXPathOp::XPathOpRangeto => {
                write!(output, "RANGETO").ok();
            } // _ => {
              //     write!(output, "UNKNOWN %d\n", (*op).op);
              //     return;
              // }
        }
        writeln!(output).ok();
        // finish:
        if (*op).ch1 >= 0 {
            xml_xpath_debug_dump_step_op(
                output,
                comp,
                &raw const comp.steps[(*op).ch1 as usize] as _,
                depth + 1,
            );
        }
        if (*op).ch2 >= 0 {
            xml_xpath_debug_dump_step_op(
                output,
                comp,
                &raw const comp.steps[(*op).ch2 as usize] as _,
                depth + 1,
            );
        }
    }
}

/// Dumps the tree of the compiled XPath expression.
#[doc(alias = "xmlXPathDebugDumpCompExpr")]
pub unsafe fn xml_xpath_debug_dump_comp_expr<'a>(
    output: &mut (impl Write + 'a),
    comp: &XmlXPathCompExpr,
    depth: i32,
) {
    unsafe {
        let shift = "  ".repeat(depth.clamp(0, 25) as usize);

        write!(output, "{}", shift).ok();

        if comp.stream.is_some() {
            writeln!(output, "Streaming Expression").ok();
        } else {
            writeln!(
                output,
                "Compiled Expression : {} elements",
                comp.steps.len(),
            )
            .ok();
            xml_xpath_debug_dump_step_op(
                output,
                comp,
                &raw const comp.steps[comp.last as usize] as _,
                depth + 1,
            );
        }
    }
}
