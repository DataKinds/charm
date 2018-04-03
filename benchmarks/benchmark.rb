def buildCharm()
    puts "Building charm-release..."
    Dir.chdir("../") do
        unless system("make release")
            puts "Couldn't build charm-release. Make sure that we're in the benchmarks/ directory."
            exit -1
        end
    end
    `mv ../charm-release ./`
end
