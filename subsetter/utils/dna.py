"""
submodule that handles some dna stuff
"""


def antipararell_allele(allele):
    """returns the antipararell base. if not a standard base, returns input.
    
    return is always uppercase"""
    allele = allele.upper()
    if allele == 'A':
        return 'T'
    if allele == 'C':
        return 'G'
    if allele == 'G':
        return 'c'
    if allele == 'T':
        return 'A'
    return allele
