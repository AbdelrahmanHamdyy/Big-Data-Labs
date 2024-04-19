import java.io.IOException;
import org.apache.hadoop.conf.Configuration;
import org.apache.hadoop.fs.Path;
import org.apache.hadoop.io.IntWritable;
import org.apache.hadoop.io.Text;
import org.apache.hadoop.mapreduce.Job;
import org.apache.hadoop.mapreduce.Mapper;
import org.apache.hadoop.mapreduce.Reducer;
import org.apache.hadoop.mapreduce.lib.input.FileInputFormat;
import org.apache.hadoop.mapreduce.lib.output.FileOutputFormat;

public class CustomerTotalMoney {

    public static class CustomerTotalMoneyMapper
            extends Mapper<Object, Text, Text, IntWritable> {

        private final static IntWritable money = new IntWritable();
        private Text customerId = new Text();

        public void map(Object key, Text value, Context context
        ) throws IOException, InterruptedException {
            // Here we will split the given value(input) by , as it's a csv file
            // so the first part will be the customer Id 
            // and the second item will be the amount spent
            String[] parts = value.toString().split(",");

            // Extracting customer Id and Budget spent
            String customer_id = parts[0].trim();
            int budget_spent = Integer.parseInt(parts[1].trim());

            // Setting the extracted values to the values of the mapper class
            customerId.set(customer_id);
            money.set(budget_spent);

            // Finally sent the result of the mapping operation to the intermediate level
            // which in its role makes data ready for the reduce stage
            context.write(customerId, money);
        }
    }

    public static class MoneySumReducer
            extends Reducer<Text, IntWritable, Text, IntWritable> {

        private IntWritable result = new IntWritable();

        public void reduce(Text key, Iterable<IntWritable> values,
                           Context context
        ) throws IOException, InterruptedException {
        // Same as the default reduce function which is considered a summation of all values
        // that come with the same key so it's the same as the WordCount famous example
            int sum = 0;
            for (IntWritable val : values) {
                sum += val.get();
            }
            result.set(sum);
            //Then we return here the final output which will be the key value which is the
            // customer Id with its result which is the total money spend
            context.write(key, result);
        }
    }

    public static void main(String[] args) throws Exception {
        Configuration conf = new Configuration();
        Job job = Job.getInstance(conf, "customer budget");
        job.setJarByClass(CustomerTotalMoney.class);
        job.setMapperClass(CustomerTotalMoneyMapper.class);
        job.setCombinerClass(MoneySumReducer.class);
        job.setReducerClass(MoneySumReducer.class);
        job.setOutputKeyClass(Text.class);
        job.setOutputValueClass(IntWritable.class);
        FileInputFormat.addInputPath(job, new Path(args[0]));
        FileOutputFormat.setOutputPath(job, new Path(args[1]));
        System.exit(job.waitForCompletion(true) ? 0 : 1);
    }
}
