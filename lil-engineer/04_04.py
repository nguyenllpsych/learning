##import required libraries
import pyspark

##create spark session
spark = pyspark.sql.SparkSession \
   .builder \
   .appName("Python Spark SQL basic example") \
   .config('spark.driver.extraClassPath', "/Users/harshittyagi/Downloads/postgresql-42.2.18.jar") \
   .getOrCreate()


##read table from db using spark jdbc
movies_df = spark.read \
   .format("jdbc") \
   .option("url", "jdbc:postgresql://localhost:5432/etl_pipeline") \
   .option("dbtable", "movies") \
   .option("user", "<username>") \
   .option("password", "<password>") \
   .option("driver", "org.postgresql.Driver") \
   .load()
   
##add code below
user_df = spark.read \
   .format("jdbc") \
   .option("url", "jdbc:postgresql://localhost:5432/etl_pipeline") \
   .option("dbtable", "_____") \
   .option("user", "<username>") \
   .option("password", "<password>") \
   .option("driver", "______") \
   .load()

##print the users dataframe
print(______)




