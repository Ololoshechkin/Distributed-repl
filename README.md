# Distributed repl

The project is distributed and is composed by the following components: 

1. Client with UI:
* Code text field
* Program result label
* Buttons: compile, open, repl mode, save и quit: 

![Image 1](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image1.png)
![Image 2](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image2.png)
![Image 3](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image3.png)
 

If any error (network error/parsing error/compilation error) happens there is an error message window for that:


![Image 4](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image4.png)
![Image 5](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image5.png)
![Image 6](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image6.png)


A client parses source code written on a programming language designed by myself, then the client serializes the AST of the code and sends to master.

Also (new feature) there is a Repl Mode that allows client to send code fragments (statements or expressions) to execute/evaluate. In case of statements a Repl state of the client changes remotely:

![Image 8](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image8.png)

![Image 9](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image9.png)

Grammar of my language that I support:

* Program = (Statement*) ReturnStatement
* Statement = (Assignment | Loop | Invocation | DBComand)
* Assignment = 'var' Identifier '=' Expression
* Loop = 'while' BracesExpression '{' (Statement*) '}' 
* Expression = Invocation | Constant | OperatorExpression | IfThenElseExpression | LambdaDefExpression | BracesExpression
* Invocation = Identifier '(' Expression (',' Expression)* ')'
* Identifier = [a-z|A-Z]+ (note : names like "function", "return", "if" or other keywords are not allowed. If you use such names nothing can be guaranteed but in most SANE! cases they do work!)
* Constant = IntConstant | StringConstant | BoolConstant
* IntConstant = [0-9]+ (note: 00091 <=> 91)
* StringConstant = \"[a-z0-9A-Z_]*\"
* BoolConstant = 'true' | 'false' 
* OperatorExpression : UnaryOpExpr | BinaryOpExpr
* UnaryOpExpr = UnaryOp BracesExpression
* BinaryOpExpr = BracesExpression BinaryOp BracesExpression
* UnaryOp : '+' | '-' | '!'
* BinaryOp : '+' | '-' | '*' | '/' | '==' | '!=' | '<' | '&&' | '||' | '::' | '##'

Note: '::' is an equivalent of integer operator '==' but for Strings

Note: '##' is an equivalent of integer operator '+' but for Strings

* IfThenElseExpression = 'if' BracesExpression 'then' BracesExpression 'else' BracesExpression
* LambdaDefExpression = 'function' '(' (Identifier (',' Identifier)*)? ')' '{' Program '}'
* BracesExpression = '(' Expression ')'
* DBComand = PublishComand | LoadComand
* PublishComand = 'PUBLISH' Key Value ReplicationFactor 
* Key = BracesExpression of type String
* Value = BracesExpression of type String
* ReplicationFactor = IntConstant
* LoadComand = 'LOAD' Key Identifier 
* Key = BracesExpression of type String
* ReturnStatement = 'return' Expression

Valid program example:


```
var             i = 0  
var s = "a"  
var inc  = function(x) {  
	var y = (x) + (1)  
	return y   
}   
var isOdd = function(x) { 
	var y = ((x)  / (2)) * (2) 
	var res = if ((x) == (y)) then (true) else (false) 
	return res 
} 
while ((i) < (5)) { 
         var i = inc(i) 
         var s = if (isOdd(i)) then ((s) ## ("c")) else ((s) ## ("y")) 
} 
PUBLISH (s) ("aaa") 1 
LOAD (s) q 
return q 
```

It's result is a database value for the key "aycycy" assigned to "aaa" and program output is "aaa".

2. Master:
Listens for new client and worker connections. Master is fully async, processes requests in parallel and supports dynamic connections/disconnections of workers.
Master reacts on the following requests:
- Receives ClienRequest (AST of a program to compile). Sends WorkerRequest(ClientRequest + ID of the client) to randomly-chosen worker that is currently availabele, then master forgets about the request.
- Receives RegisterWorker from new worker, adds it to the list of available workers.
- Receives CompilationError(description) + ID of a client. Sends CompilationError(description) to the client with ID.
- Receives Result(program output) + ID of the client. Sends Result(program output) to the client with ID.

3. Worker:
Connects to Master with sending RegisterWorker request.
Keeps waiting for requests from Master
Does fork on each request fom Master and executes given program. Also Worker talks to a Database if needed.
When the result is ready Worker sends it to master. The result is either CompilationError(description) or ProgramResult(value)
In case Worker gets Repl-request it reads initial Repl state from Database (Repl state contains all the enviroment including defined functions) and after execution any code in repl mode the worker writes new Repl state to the Database. This allows us to execute each repl block on any worker regardles of what was the previous one for this client.

4. Database. Supports 4 types of queries: 
* publish key value replication - writes (key, value) to the key-value storage replicated to at most n=replication machines.
* load key - reads (value) from key-value storage
* downloadMap cid  - reads (map of variables) from repl-state storage for the client with Id=cid
* uploadMap cid map - writes (cid, map of variables) to the repl-state of the client with Id=cid

Database is add-hock and is composed from the following layers:
4.1 DB-master.
In case of getting publish request DB-master propogates it to n=replication number of DB-workers (they are being chosen randomly for the sake of redundancy).
In case of getting load-query DB-master sprays the request to all of DB-workers and waits for the first reply.
Поведение download и upload запросов аналогично load и publish для replication=3
Also DB-master is able to detect new DB-workers became available in runtime.
Note: DB-master is asynchronous and client request time is guarded by timeouts.
4.2 DB-worker:
There are two implementations of DB-worker that can be both used in the same system:
4.2.1 DB-worker using file system
Listens for new requests and reads/writes to a disk.
For publish(key, value)/load(key) it works with file with a name "key" containing the corresponding value. Execution is sequential.
For publishMap(cid, map)/loadMap(cid) it works with file "%cid%.map" containing repl state.
4.2.2  DB-worker using SQlite3
Works with tables: kvmaptable and replvarmap.

Note: Database allows different clients to share common variables.

Example of a chat using Database:

![Image 7](https://github.com/Ololoshechkin/haskell_project/raw/master/screenshots/image7.png)

Notes (how to run):
1. Run master

arguments: master host, master port
```
stack run master "127.0.0.1" "8999"
```
2. Run DB-master
	
arguments: dbmaster host, dbmaster port
```
stack run dbmaster "127.0.0.1" "9997"
```
3. Run Worker(s) (any number of workers >= 1 is fine)
	
arguments: master address (is printed in master shell), dbmaster address (is printed in DB-master shell), port for connecting to master, port for connecting to DB-master
```
stack run worker "127.0.0.1:8999:0" "127.0.0.1:9997:0" "2998" "1999"
```
4. Run DB-workre(s) (any number of them >= 1)

arguments: dbmaster address, port for connecting to DB-master, "fs" or "sql" -- implementation of DB-worker
```
stack run dbworker "127.0.0.1:9997:0" "6995" fs
```
5. Run client(s)
	
arguments: master address, port for listening to Master
```
stack run client "127.0.0.1:8999:0" "7777"
```

Notes (reliability):
* In case worker dies the system is still alive and works fine. In case of any other worker exists clients switch to it.
* If the last worker is dead --- system is still alive but clients don't get any result from master and show "Timeout exceeded" window.
* If a DB-worker dies --- system still works fine and most likely no data is lost in case it was written with a propper replication.
* DB-worker can be recovered with the same state.
* In case DB-master dies all DB-workers die. But Workers can still compile programs without Database queries.
* In case Master dies all workers die. Client shows message that Master is unavailable and the client can't do anything useful from now on.

Touched technologies:
* UI (qtah)
* Distributed systems
* Network-tcp
* File system usage
* Database (SQLite)

Used language features:
* Monads: IO, MonadRandom, ParIO
* Monad transformers: ReaderT
* Parser-combinators (megaparsec)
* Parallelism and Concurrency: PAR, fork, IVar, MVar, TVar and operations with them
* Lenses: Template haskell+lenses for Server state.
