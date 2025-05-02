use crate::{
    parser::{XmlParserCtxt, XmlParserCtxtPtr, XmlParserInput, XmlParserNodeInfo, XmlSAXHandler},
    tree::{XmlDocPtr, XmlNodePtr},
};

pub mod parser;
mod taginfo;
pub mod tree;

// Most of the back-end structures from XML and HTML are shared.
pub type HtmlParserCtxt<'a> = XmlParserCtxt<'a>;
pub type HtmlParserCtxtPtr<'a> = XmlParserCtxtPtr<'a>;
pub type HtmlParserNodeInfo = XmlParserNodeInfo;
pub type HtmlSAXHandler = XmlSAXHandler;
// pub type HtmlSAXHandlerPtr = XmlSAXHandlerPtr;
pub type HtmlParserInput<'a> = XmlParserInput<'a>;
pub type HtmlDocPtr = XmlDocPtr;
pub type HtmlNodePtr = XmlNodePtr;
