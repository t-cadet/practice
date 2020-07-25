# Sirens Stats

Small data analysis project: given a list of sirens, outputs the number of unique and duplicated sirens. 

The frontend version runs with JS & JQuery; the backend version with Haskell.

## Getting started (Linux)

Clone the repo:

```bash
git clone https://github.com/tristanCadet/practice
```

Remove irrelevant folders:

```
cd practice
shopt -s extglob
rm -r -- !(sirens-stats)
```

### Frontend

Go to the frontend directory and:

- Open the `.html` file in a browser
- Click on the `Get stats` button
  ![get stats demo](https://i.imgur.com/EgXN4dk.png)

### Backend
- Install Haskell Stack (https://docs.haskellstack.org/en/stable/README/)
- In the backend directory do:

```bash
stack build # to install the dependencies and compile the project
stack run # to run the project
stack test # to run the tests
```

Example run:

```bash
practice/sirens-stats/backend> stack run
(77442,2256)
```



