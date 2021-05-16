package aba.framework


class Framework private (rules: Set[Rule],
                         assumptions: Set[Literal],
                         contraries: Set[Contrary],
                         goals: Set[Literal],
                         constraints: Set[Literal])
