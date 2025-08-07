# Synchronic Web Ontology Compose Network

This repository contains materials to deploy a single smart contract journal using Docker Compose

## Requirements

- Docker
- Docker Compose (currently developed on v2.26.1)

## Configuration

Please set the following environmental variables to configure the notary journal.

- `SECRET` (required): a string used to generate authentication credentials
- `PORT`: port number to forward on the host machine (default:8192)
- `PERIODICITY`: a nonnegative integer that determines the period of each synchronization step where period = 2 ^ PERIODICITY

## Start

`$ SECRET=password PORT=80 docker compose up`

## End

`$ docker compose down -v`

## Usage

### Account Creation:
In order to use any of the smart contract functions, an account must be created. Create an account using the following call in the interface.
`(*local* "password" (create-account "your_username" "your_password" #f))`
Upon creation, each account will be initialized with a set amount of compute tokens (this number can be set by anyone with the appropriate access). 

### Contract Deployment:
To deploy a contract, use the following format. The path to the contract must begin with `*state*`. Contract definitions must be wrapped in a begin statement. Variables must be defined first, as a hash table called vars, with variables initialized to whatever values are necessary. #f can be used as an init value. There is no restriction on function names. When referring to variables in a function definition, refer to them as elements of the hash table, as shown in the below code. Functions can use the caller's username (shown in example ):
```
(*local* "password" 
    (contract-deploy "your-username" "your-password" (path-to-contract) 
                     (begin (define vars (hash-table 'var-name var-value 'var-name2 var-value2)) 
                            (define function-name (lambda (function-parameters) (function-definition))) 
                     ))) 
```
An example is below:
```
(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts bill2) 
                     (begin (define vars (hash-table 'votes 0 'voters (hash-table))) 
                            (define vote2 (lambda () (if (eq? (vars 'voters username) #f) 
                                                         (begin (set! (vars 'voters username) #t) (set! (vars 'votes) (+ 1 (vars 'votes)))) 
                                                         (error "you already voted"))
                     )))))
```
If a function needs to make a cross-contract call, it can be done as follows, using the cross-call subfunction. In the example below, cross-call is called with three parameters: the path to the contract being called, the index (default #f), and the name of the method in the contract being called. In the example below, the vote-similar method calls the vote2 method, from the contract above.
```
(*local* "password" 
    (contract-deploy "divya" "passwd" (*state* contracts bill1) 
                     (begin (define vars (hash-table 'votes 0 'voters (hash-table))) 
                            (define vote-similar (lambda () (cross-call '(*state* contracts bill2) #f '(vote2)))) 
                     )))
```

### Calling a Contract:
To call a contract, use the format below:
```
(*local* "password"  
    (contract-call "your-username" "your-password" 
                   (path-to-contract) 
                   #f 
                   (method-name))) 
```

Here is an example where a user calls the vote2 method defined above.
```
(*local* "password"  
    (contract-call "user1" "pass1" 
                   (*state* contracts bill2) 
                   #f 
                   (vote2))) 
```

### Compute Tokens:
Deploying and calling contracts require an account because each operation costs an amount of tokens proportional to the number of cpu cycles consumed by the operation. Tokens will be replenished to the starting amount when the replenishment period has passed (the replenishment period can be set by anyone with the appropriate access).

