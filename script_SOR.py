import subprocess
import re
import os
# import matplotlib.pyplot as plt
# import numpy as np

# Valeurs à tester 
mesh = [120]
#mesh = [120]
proc = [1,2,3,4,5,6,8]
#proc = [1,2]
L_temps_execution = [[0 for i in range(len(proc))] for j in range(len(mesh))]
U_exact_calcule = [[[] for i in range(len(proc))] for j in range(len(mesh))]
U_calcule = [[[] for i in range(len(proc))] for j in range(len(mesh))]

# lecture des données et sauvegarde
fichier = open("poisson.data", "r")
data = fichier.readlines()
data2 = data
fichier.close()

commande_bash = "make clean"
result = subprocess.run(commande_bash, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)
print(result.stdout)
commande_bash = "make"
subprocess.run(commande_bash, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

for k in range(len(mesh)): 
    for l in range(len(proc)):
        print("Mesh: ", mesh[k], "Proc: ", proc[l])
        fichier = open("poisson.data", "w")
        for i in range(len(data)):
            if i == 0:  
                fichier.write(str(mesh[k])+" !"+"\n")
                #print("zizi")
            elif i == 1:   
                fichier.write(str(mesh[k])+" !"+"\n")
        fichier.close()

        # Commande Bash qui exécute le script.sh
        commande_bash = f"mpiexec -n {proc[l]} --allow-run-as-root --use-hwthread-cpus poisson"  

        # Exécute la commande Bash et capture la sortie
        result = subprocess.run(commande_bash, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

        # Vérifie si la commande s'est exécutée avec succès (code de retour 0)
        if result.returncode == 0:
            # Recherche du motif du temps d'exécution dans la sortie d'erreur
            temps_execution = re.search(r'(\d\.\d+E[+-]?\d+)', result.stdout)
            if temps_execution:
                # Récupère le temps d'exécution complet (y compris les minutes et les secondes)
                temps_execution_str = temps_execution.group(1)
                print("Temps d'exécution complet :", temps_execution_str)
                L_temps_execution[k][l] = float(temps_execution_str)

                pattern = re.compile(r'u_exact=\s+([\d.E+-]+)\s+u\s+=\s+([\d.E+-]+)')
                matches = pattern.findall(result.stdout)

                U_exact_calcule[k][l] = [float(match[0]) for match in matches]
                U_calcule[k][l] = [float(match[1]) for match in matches]
            
            else:
                print("Le temps d'exécution n'a pas été trouvé dans la sortie d'erreur.")
                print("Sortie complète :")
                print(result.stdout)
        else:
            # Affiche les erreurs si la commande a échoué
            print("La commande Bash a échoué. Erreurs :")
            print(result.stderr)

# Nettoyage des fichiers temporaires
commande_bash = "make clean"
subprocess.run(commande_bash, shell=True, stdout=subprocess.PIPE, stderr=subprocess.PIPE, text=True)

fichier = open("poisson.data", "w")
for i in range(len(data2)):
    fichier.write(data2[i])
fichier.close()

# Write data to file
fichier = open("data.csv", "w")
for k in range(len(mesh)):
    for l in range(len(proc)):
        fichier.write("")
        fichier.write("New_data" + " " + str(mesh[k]) + " " + str(proc[l]) + " " + str(L_temps_execution[k][l]) + "\n")
        for i in range(len(U_exact_calcule[k][l])):
            fichier.write(str(U_exact_calcule[k][l][i]) + " " + str(U_calcule[k][l][i]) + "\n")
fichier.close()



