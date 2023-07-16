# Insegurança alimentar na região Nordeste

Este repositório contém um conjunto de rotinas em R destinadas à importação, manipulação e extração de análises dos microdados da POF para a região Nordeste do Brasil. Num primeiro momento, desenvolvem-se diversas estatísticas descritivas da amostra para a região em questão e, após isto, desenvolve-se um modelo de regressão logística baseada em algumas variáveis, buscando-se identificar os determinantes da insegurança alimentar na região.

Para identificar os determinantes da insegurança alimentar na região Nordeste, propõem-se o seguinte modelo de regressão logística:

$$
\begin{aligned}
Pr(insalim = 1) =
\frac{exp(\beta_0 + \beta_1rendafam + \beta_2homem + \beta_3sexo \cdot raca + \beta_4 instrucao + \beta_5idade + \beta_6zona + \beta_7composicao)}{1 + exp(\beta_0 + \beta_1rendafam + \beta_2homem + \beta_3sexo \cdot raca + \beta_4 instrucao + \beta_5idade + \beta_6zona + \beta_7composicao)}
\end{aligned}
$$

Em que:

-   $Pr(insalim = 1)$ é a probabilidade de o indivíduo $i$ da amostra ser pobre dado um conjunto de $k$ variáveis explicativas;
-   $\beta_0$ é a constante do modelo;
-   $rendafam$ é a renda total da família;
-   $homem$ é uma variável binária que é igual a 1 se o indivíduo for homem e 0, caso contrário;
-   $sexo \cdot raca$ é uma interação entre as variáveis categóricas de gênero e raça;
-   $instrucao$ corresponde aos anos de estudo de um indivíduo $i$ da amostra;
-   $idade$ corresponde à idade do indivíduo;
-   $zona$ é uma variávei binária que é igual a 1 se o domicílio se localiza em uma zona urbana e 0, caso contrário;
-   $composicao$ corresponde à quantidade de membros que o domicílio possui;
-   $\beta_1, \dots, \beta_7$ são os coeficientes a serem estimados.
