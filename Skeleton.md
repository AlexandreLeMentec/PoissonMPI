
# STRUCTURE OF THE CODE AND ADVANCEMENT
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
>> Divide procs in X and Y **&#9744;**

> ***domaine*** <span style="color:#85d0df">(subroutine)</span>

> ***voisinage*** <span style="color:#85d0df">(subroutine)</span>

> ***type_derive*** <span style="color:#85d0df">(subroutine)</span>

> ***communication*** <span style="color:#85d0df">(subroutine)</span>

> ***erreur globale*** <span style="color:#dfad85">(function)</span>

> ***ecrire_MPI*** <span style="color:#85d0df">(subroutine)</span>

> ***finalisation_mpi*** <span style="color:#85d0df">(subroutine)</span>

    - calcul_poisson.f90
>

    - read.f90
