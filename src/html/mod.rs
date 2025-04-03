use crate::{
    libxml::parser::{XmlSAXHandler, XmlSAXHandlerPtr},
    parser::{XmlParserCtxt, XmlParserCtxtPtr, XmlParserInput, XmlParserNodeInfo},
    tree::{XmlDocPtr, XmlNodePtr},
};

pub mod parser;
mod taginfo;
pub mod tree;

// Most of the back-end structures from XML and HTML are shared.
pub type HtmlParserCtxt = XmlParserCtxt;
pub type HtmlParserCtxtPtr = XmlParserCtxtPtr;
pub type HtmlParserNodeInfo = XmlParserNodeInfo;
pub type HtmlSAXHandler = XmlSAXHandler;
pub type HtmlSAXHandlerPtr = XmlSAXHandlerPtr;
pub type HtmlParserInput = XmlParserInput;
pub type HtmlDocPtr = XmlDocPtr;
pub type HtmlNodePtr = XmlNodePtr;
