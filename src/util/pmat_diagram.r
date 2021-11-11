library(DiagrammeR)

grViz("
  digraph pmat_workflow {
    graph [overlap = true, fontsize = 10]

    node [shape = box, fontname = Helvetia]
    A;B

    node [shape = circle, fixedsize = true, width = 0.5]
    A.1; A.2; A.3;
    B.1; B.2; B.3; B.4

    node [shape = point]
    1;2

    1 -> A A -> B B -> 2
    A -> A.1 A.1 -> A.2 A.2 -> A.3
    B -> B.1 B.1 -> B.2 B.2 -> B.3 B.3 -> B.4
  }
")