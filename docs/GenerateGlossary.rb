require "yaml"
require "cgi"

def generateArgument(a, si)
    out = ""
    out << "<dt>Stack index #{si}: #{a["type"]}</dt>"
    out << "<dd>#{a["desc"]}</dd>"
    return out
end

def generateQuickUsage(f)
    name = f.keys.first
    out = ""
    if !f[name]["pops"].nil?
        f[name]["pops"].each do |a|
            out << "<i>#{a["type"]}</i> "
        end
    end
    out << name << " "
    if !f[name]["pushes"].nil?
        out << "        => "
        f[name]["pushes"].each do |a|
            out << "<i>#{a["type"]}</i> "
        end
    end
    return out
end

def generateFunction(f)
    name = f.keys.first
    out = ""
    out << "<h3 id=\"#{CGI::escapeHTML(name)}-id\" class=\"function\">#{name}</h3>"
    out << "<h4>Description</h4>"
    out << "<div class=\"info\">#{f[name]["desc"]}</div>"
    if !f[name]["note"].nil?
        f[name]["note"].each do |n|
            out << "<div class=\"info\">NOTE: #{n}</div>"
        end
    end
    out << "<h4>Quick Usage View</h4>"
    out << "<div class=\"code\">#{generateQuickUsage(f)}</div>"
    if !f[name]["pops"].nil?
        out << "<div><h4>Pops</h4><dl class=\"info\">"
        f[name]["pops"].each_with_index do |a, i|
            out << generateArgument(a, f[name]["pops"].length - (i + 1))
        end
        out << "</dl></div>"
    end
    if !f[name]["pushes"].nil?
        out << "<div><h4>Pushes</h4><dl class=\"info\">"
        f[name]["pushes"].each_with_index do |a, i|
            out << generateArgument(a, f[name]["pushes"].length - (i + 1))
        end
        out << "</dl></div>"
    end
    out << %Q~
<div class="code-container">
    <button class="code-button">
        Source (click to open/close)
    </button>
    <pre class="code code-drawer">#{CGI::escapeHTML(f[name]["source"])}</pre>
</div>~
    return out
end

def generateCategory(c)
    out = "<h3>#{c["category"]}</h3>"
    c["functions"].each do |f|
        out << generateFunction(f) << "\n"
    end
    return out
end

def generateLinks(cs)
    out = ""
    cs.each do |c|
        out << "<h3 class=\"function\">#{c["category"]}</h3>"
        c["functions"].each do |f|
            out << "<a href=\"\##{CGI::escapeHTML(f.keys.first)}-id\">#{f.keys.first}</a> "
        end
    end
    return out
end

glossary = YAML.load_file("Glossary.yaml")

puts %Q~
<!DOCTYPE html>
<meta charset="utf-8" />
<html>
    <head>
        <title>
            Charm Function Glossary
        </title>
        <script type="text/javascript" src="charm.js"></script>
        <script type="text/javascript" src="expose_charm.js"></script>
        <script type="text/javascript">
            function codeClick(e) {
                var elem = e.target;
                var codePre = elem.parentElement.getElementsByClassName("code-drawer")[0];
                if (codePre.classList.contains("code-open")) {
                    codePre.classList.remove("code-open");
                    //codePre.style.height = "0";
                } else {
                    codePre.classList.add("code-open");
                    //var lineHeight = parseInt(window.getComputedStyle(codePre).lineHeight);
                    //codePre.style.height = (codePre.innerHTML.split("\\n").length * lineHeight) + "px";
                }
            }
            function replOnKeyDown(e) {
                if (e.keyCode == 13) {
                    var i = document.getElementById("replInput");
                    runCharm(i.value);
                    i.value = "";
                }
            }
            function init() {
                var codes = document.getElementsByClassName("code-button");
                for (let code of codes) {
                    code.onclick = function (e) { codeClick(e); };
                }
                document.getElementById("replInput").onkeydown = function (e) { replOnKeyDown(e); };
                initCharm();
            }
            window.onload = init;
        </script>
        <style type="text/css">
            html {
                background-color: #ddd;
                font-family: sans-serif;
            }
            body {
                width: 80%;
                min-width: 30em;
                margin: 0 auto;
                padding: 4em;
                padding-top: 2em;
                box-shadow: 0 0 100px #666;
                background-color: #fff;
            }
            h2.function-header {
                border-bottom: 2px solid #000;
            }
            .index-header {
                width: 100%;
                text-align: center;
            }
            h3 {
                border-bottom: 1px solid #000;
            }
            h3.function {
                border-bottom: 1px dotted #000;
            }
            h3.function:target {
                background-color: yellow;
            }
            #replOutput {
                margin: 0 auto;
                width: 100%;
                height: 16em;
                padding: 8px;
                border-width: thin;
                border-color: black;
                border-style: solid;
                border-radius: 5px
                background-color: #ccc;
                font-family: monospace;
                overflow-y: scroll;
            }
            #replInput {
                width: calc(100% - 15em);
            }
            .info {
                padding-left: 2em;
                margin-top: 0.5em;
                margin-bottom: 0.5em;
            }
            .code {
                overflow-y: hidden;
                background-color: #ccc;
                font-family: monospace;
                padding: 1em;
                white-space: pre;
            }
            .code-drawer {
                height: 0;
                padding: 0;
                margin: 0;

                transition: padding 0.5s cubic-bezier(0, 1, 0, 1);
                transition: margin 0.5s cubic-bezier(0, 1, 0, 1);
            }
            .code-open {
                padding: 1em;
                margin-top: 0.75em;
                margin-bottom: 0.75em;
                height: auto;
            }
        </style>
    </head>
    <body>
        <h1>
            Charm Function Glossary
        </h1>
        <p>
            This page was autogenerated by docs/GenerateGlossary.rb on #{Time.now}. It lists all of the functions in the Charm glossary in an easy to read, useful reference format.
        </p>
        <p>
            If you've stumbled across this page on accident, please feel free to check out Charm, a stack-based functional programming language at <a href="https://github.com/aearnus/charm">https://github.com/aearnus/charm</a>. It's free, terse, paradigm-smashing, and fun to use and think in.
        </p>
        <h2>
            Try Charm!
        </h2>
        <pre id="replOutput">#{`charm -v`} built using emscripten.</pre>
        <label id="replInputLabel" for="replInput">
            Charm (stack omitted)$
        </label>
        <input type="text" name="replInput" id="replInput" placeholder="Type Charm code here and press return...">
        <h2 class="index-header">
            Quick Function Index
        </h2>
        <div>
            <div style="float:left;width:45%">
                <h3 class="index-header">
                    Native Functions
                </h3>
                #{generateLinks(glossary["native_functions"])}
            </div>
            <div style="float:right;width:45%">
                <h3 class="index-header">
                    Prelude Functions
                </h3>
                #{generateLinks(glossary["prelude_functions"])}
            </div>
            <div style="clear:both"></div>
        </div>
        <h2 class="function-header">
            Native Functions
        </h2>
        #{glossary["native_functions"].map { |c| generateCategory c }.join("\n")}
        <h2 class="function-header">
            Prelude Functions
        </h2>
        #{glossary["prelude_functions"].map { |c| generateCategory c }.join("\n")}
</html>~
