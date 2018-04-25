
def quarters(qRemaining, nDay):
    if nDay==0: return qRemaining
    if qRemaining % 6: return None
    qPrior = nDay + 7*qRemaining/6
    return quarters(qPrior, nDay-1)

solutions = filter(lambda x:x[1] != None,
                   [(nDays,quarters(0,nDays))
                    for nDays in range(4,10**7)])

print ' '.join(map(str,solutions))
