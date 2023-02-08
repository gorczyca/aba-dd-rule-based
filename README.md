# flexABle
![CI](https://github.com/gorczyca/aba-dd-rule-based/actions/workflows/scala.yml/badge.svg)
___

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
  <B>:
  
  <goals and contraries of culprits without complete arguments for proponent>
  
  <defences>
  
  <culprits>
 
  
[Information who won the game if applicable]  
```

e.g.:

## Inputs
Press one of the following button followed by `Enter`:
 - `?` - print possible moves in current situation, grouped by the type of the move.
 - `s` | `show` - print information  about current dispute state
 - `f` - perform random move forward
 - `f <N>` - perform `N` random moves forward (if possible)
 - `h` | `help` - show help
 - `d` - show info about decorators
 - `legend` - generate a DOT file explaining used shapes and colours
 - `b` - backtrack once
 - `bb` - backtrack to the very beginning (initial state)
 - `b <N>` - backtrack `N` times (if possible)
 - `q` | `quit` - quit
 - `a` - switch to argument-based dispute state representation **(does not work correctly yet)**
 - `<move type>` - perform random move of type `move type`
 - `<move type> <index>` - perform a move of type `move type` and of index `index` (according to the moves listed when inputting `?`)
 - `dot [s]`  - generate a DOT graph representation based on rule-based graph to a file. When used with `s` (optional), solid colours are used for fillings instead of gradients
 - `dot [s] <filename>` - generate a DOT graph representation based on rule-based graph to `<filename>` file
 - `i` | `info` - print information about currently set **dispute advancement type** and **termination criteria type**
 - `ca <dispute advancement type>` - set the dispute advancement type to `<dispute advancement type>` 
 - `ct <termination criteria type>` - set the termination criteria type to `<termination criteria type>`
 
#### Possible `<move type>`s are (case insensitive): 
`PB1`, `PB2`, `PF1`, `PF2`, `OB1`, `OB2`, `OF1`, `OF2`

#### Possible `<dispute advancement type>`s are (case insensitive): 
`DF`, `DAB`, `DABF`, `DC`, `DS`

#### Possible `<termination criteria type>`s are (case insensitive): 
`TA`, `TC`, `TS`

# Generated graphical output example (converted from DOT to PNG)
![ds6_gradient](https://user-images.githubusercontent.com/43420734/125137442-fa4adf00-e10c-11eb-9123-cfd036899dee.png)

where the meaning of shapes and colours is as follows:

![legend](https://user-images.githubusercontent.com/43420734/125137482-0afb5500-e10d-11eb-94d4-8df46ae7abe6.png)

