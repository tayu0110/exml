import { DOMParser } from "jsr:@b-fuze/deno-dom";
import * as path from "jsr:@std/path";
import { existsSync } from "jsr:@std/fs";
import { parseArgs } from "jsr:@std/cli/parse-args";

const flags = parseArgs(Deno.args, {
    string: ["cookie"],
});

if (!flags.cookie) {
    console.error("--cookie is required.");
    Deno.exit(1);
}

const REPOSITORY_ROOT: string =
    "https://dev.w3.org/cvsweb/2001/DOM-Test-Suite/";
const TEST_SUITE_ROOT: string = "./DOM-Test-Suite";
const HEADER = {
    headers: {
        "User-Agent":
            "Mozilla/5.0 (X11; Ubuntu; Linux x86_64; rv:138.0) Gecko/20100101 Firefox/138.0",
        "Cookie": flags.cookie,
    },
};

// Initialize Test Suite root.
if (existsSync(TEST_SUITE_ROOT)) {
    Deno.removeSync(TEST_SUITE_ROOT, { recursive: true });
}
Deno.mkdirSync(TEST_SUITE_ROOT);
Deno.chdir(TEST_SUITE_ROOT);

async function fetch_test_suite(uri: string) {
    const page = await fetch(uri, HEADER);
    const text: string = await page.text();
    const doc = new DOMParser().parseFromString(text, "text/html");
    const table = doc.querySelector("table[class='dir']");
    let row = table?.firstElementChild?.firstElementChild;
    // prevent 429 (Too Many Requests)...
    setTimeout(() => {}, 1000);

    while (row != null) {
        if (row.tagName.toLowerCase() != "tr") {
            row = row.nextElementSibling;
            continue;
        }

        const td = row.firstElementChild;
        const cl = td?.getAttribute("class");
        if (cl == "dir") {
            const a = td?.firstElementChild?.nextElementSibling;
            const href = a?.getAttribute("href");
            if (href && href != "../") {
                Deno.mkdirSync(href);
                Deno.chdir(href);
                const next_path = URL.parse(uri + href)?.toString();
                if (next_path) {
                    await fetch_test_suite(next_path);
                }
            }
        } else if (cl == "file") {
            const a = td?.nextElementSibling?.firstElementChild;
            const href = a?.getAttribute("href");
            if (href) {
                const fpath = URL.parse(
                    uri + href + ";content-type=application/octet-stream",
                );
                if (fpath) {
                    const basename = path.basename(fpath.pathname);
                    const fpage = await fetch(fpath, HEADER);
                    if (fpage.ok) {
                        const ftext = await fpage.bytes();
                        await Deno.writeFile(basename, ftext);
                    }
                }
            }
        }

        row = row.nextElementSibling;
    }
    Deno.chdir("../");
}

await fetch_test_suite(REPOSITORY_ROOT);
const junit = await fetch(
    "https://sourceforge.net/projects/junit/files/junit/3.8.1/junit3.8.1.zip",
);
await Deno.writeFile(
    `${TEST_SUITE_ROOT}/lib/junit3.8.1.zip`,
    await junit.bytes(),
);

const build_xml = new TextDecoder().decode(
    await Deno.readFile(`${TEST_SUITE_ROOT}/build.xml`),
)
    // .replaceAll('source="1.2"', 'source="8"')
    // .replaceAll('target="1.1"', 'target="8"')
    .replaceAll(
        "http://www.w3.org/2002/08/xmlspec-v22-dom.dtd",
        "https://www.w3.org/XML/1998/06/xmlspec-v21.dtd",
    )
    .replaceAll("xmlspec-v22-dom.dtd", "xmlspec-v21-dom.dtd")
    .replaceAll(
        "2000/REC-DOM-Level-2",
        "2000/PR-DOM-Level-2",
    )
    .replaceAll(
        "20001113",
        "20000927",
    );
await Deno.writeTextFile(`${TEST_SUITE_ROOT}/build.xml`, build_xml);
