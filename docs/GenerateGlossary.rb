require "yaml"

def generateFunction(f)
    name = f.keys.first
    out = ""
    out << "<h3>#{name}</h3>"
    out << "<div>#{f[name]["desc"]}</div>"
    if !f[name]["note"].nil?
        f[name]["note"].each do |n|
            out << "<div>NOTE: #{n}</div>"
        end
    end
    out << "<div>Code:<pre>#{f[name]["source"]}</pre></div>"
    out << "<div>Pops:</div>"
    out << "<div>Pushes</div>"
    return out
end

def generateCategory(c)
    out = "<h3>#{c["category"]}</h3>"
    c["functions"].each do |f|
        out << generateFunction(f) << "\n"
    end
    return out
end


glossary = YAML.load_file("Glossary.yaml")

puts """
<html>
    <head>
        <title>
            Charm Function Glossary
        </title>
    </head>
    <body>
        <h1>
            Charm Function Glossary
        </h1>
        <p>
            This page was autogenerated by docs/GenerateGlossary.rb on #{Time.now}. It lists all of the functions in the Charm glossary in an easy to read, useful reference format.
        </p>
        <h2>
            Native Functions
        </h2>
        #{glossary["native_functions"].map { |c| generateCategory c }.join("\n")}
        <h2>
            Prelude Functions
        </h2>
</html>
"""
