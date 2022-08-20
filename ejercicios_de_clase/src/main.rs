mod toss_up;
//mod stackalc;

/*
Puesta en comun:
1. para cada ejercicio agregarlo con "mod file_name;" al inicio del archivo main
2. Hacer el ejercicio en un archivo nuevo en la carpeta ejercicios_de_clase con el nombre del ejercicio
3. La funcion/es nombrarlas pub antes de la declaracion para que sean public
4. en el main.rs llamarlo en el main de este archivo con "file_name::function_name();"

Luego de terminados los ejercicios y probados comentar la llamda y dejar descomentado solo el ultimo.
*/
fn main() {
    toss_up::toss_up();
    // stackalc::stackalc();
}
