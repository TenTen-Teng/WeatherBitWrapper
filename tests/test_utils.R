df <- data.frame(Name = c("Alice", "Bob", "Charlie"), Age = c(25, 30, 35))
params <- c(
    'abc', 
    '123',
    '345'
    )

save_csv(
    dataframe = df, dir = './', 
    endpoint = 'endpoint', params = params
    )
