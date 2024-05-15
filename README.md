# Lastništvo objektov namesto avtomatskega čistilca pomnilnika med lenim izračunom

Cilj magistrske naloge je pripraviti simulator STG stroja, nato pa spremeniti STG jezik tako, da bo namesto avtomatičnega čistilca pomnilnika uporabljal model lastništva po zgledu programskega jezika Rust. Zanimalo nas bo, kakšne posledice to v STG stroj prinese, kakšne omejitve se pri tem pojavijo ter do kakšnih problemov lahko pri tem pride. Zavedati se moramo, da obstaja možnost, da koncepta lastništva ni mogoče vpeljati v STG stroj brez korenitih sprememb zasnove stroja samega - v tem primeru bomo podali analizo, zakaj lastništva v STG stroj ni mogoče vpeljati.

## Vsebina repozitorija

| Lokacija | Opis |
| --- | --- |
| [`crochet`](crochet/) | Prevajalnik za len funkcijski programski jezik STG, ki namesto avtomatskega čistilca pomnilnika uporablja princip lastništva. |
| [`magistrsko_delo`](magistrsko_delo/) | Magistrsko delo v pdf obliki in vse LaTeX datoteke, potrebne za generiranje le-tega. |
| [`literatura`](literatura/) | Uporabna literatura. |
| [`dispozicija`](dispozicija/) | Prijava teme za magistrsko delo. |
| [`vmesno_porocilo`](vmesno_porocilo/) | Vmesno poročilo o poteku magistrske naloge. |
