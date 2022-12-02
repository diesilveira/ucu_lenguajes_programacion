
// Funcion para testear el asyncGoUntil
async function delay(time, res = Date.now()) {
    return await new Promise(resolve =>
        setTimeout(() => {
            resolve(res);
        }, time)
    )
}

async function asyncGoUntil(values, fun, time) {
    let results = []
    let completed = false
    let timer = new Promise(resolve =>
        setTimeout(() => {
            completed = true
            resolve()
        }, time)
    )

    for (const value of values) {
        // Race current Promise against timer
        let result = await Promise.race([fun(value), timer])
        if (completed) break
        // Store if timer is not completed
        results.push(result)
    }

    return results
}