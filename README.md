# aba-dd-rule-based

## Running (download jar from the "Releases" section, then):

```
java -jar <jar name> <input file> [-i <input format>]
```

`<input format>` is optional and defaults to `aba` (another possible format is `apx`). 
Directory [examples](examples) contains exemplary ABA frameworks.

## Interface
A dispute state is presented in the following way:

```
<dispute state number>. <move type>: <rule / assumption>
Dispute state:
  (<B>, <goals and contraries of culprits without complete arguments for proponent>, <defences>, <culprits>)
  
Assumptions:
  <assumptions>
  
Rules:
  <rules>

Move sequence:
  <move sequence>
  
[Information who won the game if applicable]  
```

e.g.:

```
11. OB1: e<-d
Dispute state:
	({$**"^a ; $**"^c ; *!d ; *d <- g ; *d <- a ; *e <- d ; $**e <- a ; $**e ; $**^f ; $**f <- c ; $**g <- e ; $**^g ; *h <- f ; *!h ; $**i <-  ; $**^i}, {}, {a ; c}, {b}) 
	
Assumptions:
	&a ; ~--b ; &c
	
Rules:
	@~d <- a ; @~d <- g ; @~e <- d ; &e <- a ; &f <- c ; &g <- e ; @~h <- f ; &i <-
	
Moves sequence:
	0. (init); 1. PB1: g <- e; 2. PF1: i <- ; 3. PF2: c; 4. OF1: f <- c; 5. PF2: a; 6. PB1: e <- a; 7. PF1: f <- c; 8. OF1: h <- f; 9. OF1: d <- g; 10. OB2: d <- a; 11. OB1: e <- d
	
Game over. Opponent won.
```

## Inputs
Press one of the following button followed by `Enter`:
 - `?` - print possible moves in current situation, grouped by the type of the move.
 - `s` / `show` - print information  about current dispute state
 - `f` - perform random move forward
 - `f <N>` - perform `N` random moves forward (if possible)
 - `b` - backtrack once
 - `bb` - backtrack to the very beginning (initial state)
 - `b <N>` - backtrack `N` times (if possible)
 - `q` - quit
 - `a` - switch to argument-based dispute state representation **(does not work correctly yet)**
 - `<move type>` - perform random move of type `move type`
 - `<move type> <index>` - perform a move of type `move type` and of index `index` (according to the moves listed when inputting `?`)
 - `dot` - generate a DOT graph representation based on rule-based graph to a file
 - `dot <filename>` - generate a DOT graph representation based on rule-based graph to `<filename>` file
 - `i` / `info` - print information about currently set **dispute advancement type** and **termination criteria type**
 - `ca <dispute advancement type>` - set the dispute advancement type to `<dispute advancement type>` 
 - `ct <termination criteria type>` - set the termination criteria type to `<termination criteria type>`
 
#### Possible `<move type>`s are (case insensitive): 
`PB1`, `PB2`, `PF1`, `PF2`, `OB1`, `OB2`, `OF1`, `OF2`

#### Possible `<dispute advancement type>`s are (case insensitive): 
`DF`, `DAB`, `DABF`, `DC`, `DS`

#### Possible `<termination criteria type>`s are (case insensitive): 
`TA`, `TC`, `TS`

