import Test.DocTest
main = doctest ["-ibench", "bench/Containers/Graph", "bench/Fgl/PatriciaTree", "bench/HashGraph/Gr"]
