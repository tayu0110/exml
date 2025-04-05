use std::sync::RwLock;

use crate::{
    error::XmlParserErrors,
    io::{__xml_loader_err, xml_check_filename, xml_ioerr},
    uri::canonic_path,
};

use super::{XmlParserCtxt, XmlParserCtxtPtr, XmlParserInput, XmlParserOption};

/// External entity loaders types.
///
/// Returns the entity input parser.
#[doc(alias = "xmlExternalEntityLoader")]
pub type XmlExternalEntityLoader =
    fn(url: Option<&str>, id: Option<&str>, context: &mut XmlParserCtxt) -> Option<XmlParserInput>;

static XML_CURRENT_EXTERNAL_ENTITY_LOADER: RwLock<XmlExternalEntityLoader> =
    RwLock::new(xml_default_external_entity_loader);

/// Changes the defaultexternal entity resolver function for the application
#[doc(alias = "xmlSetExternalEntityLoader")]
pub fn xml_set_external_entity_loader(f: XmlExternalEntityLoader) {
    *XML_CURRENT_EXTERNAL_ENTITY_LOADER.write().unwrap() = f;
}

/// Returns the xmlExternalEntityLoader function pointer
#[doc(alias = "xmlGetExternalEntityLoader")]
pub fn xml_get_external_entity_loader() -> XmlExternalEntityLoader {
    *XML_CURRENT_EXTERNAL_ENTITY_LOADER.read().unwrap()
}

/// Load an external entity, note that the use of this function for
/// unparsed entities may generate problems
///
/// Returns the xmlParserInputPtr or NULL
#[doc(alias = "xmlLoadExternalEntity")]
pub fn xml_load_external_entity(
    url: Option<&str>,
    id: Option<&str>,
    ctxt: &mut XmlParserCtxt,
) -> Option<XmlParserInput> {
    let loader = xml_get_external_entity_loader();
    if let Some(url) = url.filter(|_| xml_no_net_exists(url) == 0) {
        let canonic_filename = canonic_path(url);
        return loader(Some(&canonic_filename), id, ctxt);
    }
    loader(url, id, ctxt)
}

/// By default we don't load external entities, yet.
///
/// Returns a new allocated xmlParserInputPtr, or NULL.
#[doc(alias = "xmlDefaultExternalEntityLoader")]
pub(crate) fn xml_default_external_entity_loader(
    url: Option<&str>,
    id: Option<&str>,
    ctxt: &mut XmlParserCtxt,
) -> Option<XmlParserInput> {
    if ctxt.options & XmlParserOption::XmlParseNoNet as i32 != 0 {
        let options = ctxt.options;

        ctxt.options -= XmlParserOption::XmlParseNoNet as i32;
        let ret = xml_no_net_external_entity_loader(url, id, ctxt);
        ctxt.options = options;
        return ret;
    }
    #[cfg(feature = "catalog")]
    let resource =
        xml_resolve_resource_from_catalog(url, id, ctxt).or_else(|| url.map(|u| u.to_owned()));
    #[cfg(not(feature = "catalog"))]
    let resource = url;

    let Some(resource) = resource else {
        let id = id.unwrap_or("NULL").to_owned();
        __xml_loader_err!(ctxt, "failed to load external entity \"{}\"\n", id);
        return None;
    };
    XmlParserInput::from_filename(ctxt, &resource)
}

/// Like xmlCheckFilename but handles file URIs.
///
/// Returns 0, 1, or 2.
#[doc(alias = "xmlNoNetExists")]
fn xml_no_net_exists(url: Option<&str>) -> i32 {
    let Some(url) = url else {
        return 0;
    };

    let path = if url.starts_with("file://localhost/") {
        #[cfg(target_os = "windows")]
        {
            &url[17..]
        }
        #[cfg(not(target_os = "windows"))]
        {
            &url[16..]
        }
    } else if url.starts_with("file:///") {
        #[cfg(target_os = "windows")]
        {
            &url[8..]
        }
        #[cfg(not(target_os = "windows"))]
        {
            &url[7..]
        }
    } else {
        url
    };

    xml_check_filename(path)
}

/// Resolves the URL and ID against the appropriate catalog.
/// This function is used by xmlDefaultExternalEntityLoader and
/// xmlNoNetExternalEntityLoader.
///
/// Returns a new allocated URL, or NULL.
#[doc(alias = "xmlResolveResourceFromCatalog")]
#[cfg(feature = "catalog")]
fn xml_resolve_resource_from_catalog(
    url: Option<&str>,
    id: Option<&str>,
    ctxt: &mut XmlParserCtxt,
) -> Option<String> {
    use crate::libxml::catalog::{
        XmlCatalogAllow, xml_catalog_get_defaults, xml_catalog_resolve, xml_catalog_resolve_uri,
    };

    unsafe {
        let mut resource = None;

        // If the resource doesn't exists as a file,
        // try to load it from the resource pointed in the catalogs
        let pref: XmlCatalogAllow = xml_catalog_get_defaults();

        if !matches!(pref, XmlCatalogAllow::None) && xml_no_net_exists(url) == 0 {
            // Do a local lookup
            if matches!(pref, XmlCatalogAllow::All | XmlCatalogAllow::Document) {
                if let Some(catalogs) = ctxt.catalogs.as_mut() {
                    resource = catalogs.local_resolve(id, url);
                }
            }
            // Try a global lookup
            if resource.is_none() && matches!(pref, XmlCatalogAllow::All | XmlCatalogAllow::Global)
            {
                resource = xml_catalog_resolve(id, url);
            }
            if resource.is_none() && url.is_some() {
                resource = url.map(|url| url.to_owned());
            }

            // TODO: do an URI lookup on the reference
            if let Some(rsrc) = resource
                .as_deref()
                .filter(|resource| xml_no_net_exists(Some(resource)) == 0)
            {
                let mut tmp = None;

                if matches!(pref, XmlCatalogAllow::All | XmlCatalogAllow::Document) {
                    if let Some(catalogs) = ctxt.catalogs.as_mut() {
                        tmp = catalogs.local_resolve_uri(rsrc);
                    }
                }
                if tmp.is_none() && matches!(pref, XmlCatalogAllow::All | XmlCatalogAllow::Global) {
                    tmp = xml_catalog_resolve_uri(rsrc);
                }

                if let Some(tmp) = tmp {
                    resource = Some(tmp);
                }
            }
        }

        resource
    }
}

/// A specific entity loader disabling network accesses,
/// though still allowing local catalog accesses for resolution.
///
/// Returns a new allocated xmlParserInputPtr, or NULL.
#[doc(alias = "xmlNoNetExternalEntityLoader")]
pub fn xml_no_net_external_entity_loader(
    url: Option<&str>,
    id: Option<&str>,
    ctxt: &mut XmlParserCtxt,
) -> Option<XmlParserInput> {
    #[cfg(feature = "catalog")]
    let resource =
        xml_resolve_resource_from_catalog(url, id, ctxt).or_else(|| url.map(|u| u.to_owned()));
    #[cfg(not(feature = "catalog"))]
    let resource = url.map(|u| u.to_owned());

    if let Some(resource) = resource.as_deref().filter(|rsrc| {
        (rsrc.len() >= 6 && rsrc[..6].eq_ignore_ascii_case("ftp://"))
            || (rsrc.len() >= 7 && rsrc[..7].eq_ignore_ascii_case("http://"))
    }) {
        xml_ioerr(XmlParserErrors::XmlIONetworkAttempt, Some(resource));
        return None;
    }

    xml_default_external_entity_loader(resource.as_deref(), id, ctxt)
}
