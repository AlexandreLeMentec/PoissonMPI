# STRUCTURE OF THE CODE AND ADVANCEMENT OF THE PROJECT
___
<img src="assets\poisson-mpi-high-resolution-logo-transparent.png" alt="MarineGEO circle logo">

___
## content list by file with their state of completion

    - parallel.f90
> ***initialisation_mpi*** <span style="color:#85d0df">(subroutine)</span>
>> initialize MPI  **&#9745;** 
>
>> Know proc nb **&#9745;**
>
>> Know global nb of proc **&#9745;**


> ***creation_topologie*** <span style="color:#85d0df">(subroutine)</span>
>> Read nb of nodes in X and Y **&#9745;**
>
>> Divide procs in X and Y **&#9745;**
>
>> Create subsequent 2D grid **&#9745;** (no grid object exists so this step is just about removing periodicity)

> ***domaine*** <span style="color:#85d0df">(subroutine)</span>
>> Know the sub-domain topology's coordinates **&#9745;**
>
>> Compute the max and min indexes of the subdomain along X **&#9745; (corrected)**
>
>> Compute the max and min indexes of the subdomain along Y **&#9745; (corrected)**

> ***voisinage*** <span style="color:#85d0df">(subroutine)</span>
>> write the indexes of the subdomains's proc to the N and S subdomains **&#9745;**
>
>> write the indexes of the subdomains's proc to the E and W subdomains **&#9745;**

> ***type_derive*** <span style="color:#85d0df">(subroutine)</span>
>> Create a derived type variable for the north and south horizontal vectors **&#9744;**
>
>> Create a derived type variable for the east and west vertical vectors **&#9744;**

> ***communication*** <span style="color:#85d0df">(subroutine)</span>
>> send to North and receive from South **&#9744;**
>
>> send to South and receive from Nouth **&#9744;**
>
>> send to West and receive from East **&#9744;**
>
>> send to East ans receive from West **&#9744;**

> ***erreur globale*** <span style="color:#dfad85">(function)</span>
>> Compute local error **&#9745;**
>
>> Compute global error **&#9744;**

> ***ecrire_MPI*** <span style="color:#85d0df">(subroutine)</span>
> (NB: I did not quite catch the step of this one as I'm not very familiar with parallel writing methods. This will be corrected in due time)
>> Change the error handling wizard for files **&#9744;**
>
>> Open "donnees.dat" for writing **&#9744;**
>
>> Create derived type variable for the u matrix without the phatom points **&#9744;**
>
>> Create derived type for the final matrix which is going to be written **&#9744;**
>
>> Define the view on the file of the matrix from the start **&#9744;**
>
>> Write the u matrix with the view **&#9744;**
> 
>> Close the file **&#9744;**
>
>> clean the types **&#9744;**

> ***finalisation_mpi*** <span style="color:#85d0df">(subroutine)</span>
>> Clean the types and comms **&#9744;**
>
>> Deactivate MPI **&#9745;**


    - calcul_poisson.f90
>>

    - read.f90
>>

## functional diagram of the program
```mermaid
graph LR
var[("variables
and
files")]
var ~~~ 2([Subroutine])
2 ~~~ 3(functions)
3 ~~~ 4[/"loop"\]
4 ~~~ 5[["important operations"]]
```
___
```mermaid
graph TB
3[(poisson.data)]
-1((START)) --> 0
0[(Initialize local variables)]--> 1([initialisation_mpi])
1 --> 2([creation_topologie])
3[(poisson.data)] -.-> 2
2 --> 4([domaine])
4 --> 5([initialisation])
5 --> 6([voisinage])
6 --> 7([type_derive])
7 -.-> 8[("derivated type vectors 
N S E W")]
7 --> 9[/"DO WHILE 
.NOT. convergence
.AND.
(i < i_MAX)"\]
9 --> 10[["i = i+1"]]
10 --> 11[["u = u^{-1}"]]
11 --> 12([Communication])
12 --> 13([Calcul])
13 --> 14("diffnorm = erreur globale")
14 --> 15[["convergence = (diffnorm < eps)"]]
15 --> 16[\"END DO"/]
16 --> 9
16 --> 17(["Ecrire"])
17 --> 18([Finalisation])
18 --> 19((END))
```
