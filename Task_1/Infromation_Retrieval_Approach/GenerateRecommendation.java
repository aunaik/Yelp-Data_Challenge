/* This class generates Mean Absolute Percentage error (MAPE) in order to evaluate the methodology.
 * 
 * 
 * **/

package LuceneApproach;
import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.nio.file.Paths;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;

import org.apache.lucene.analysis.Analyzer;
import org.apache.lucene.analysis.standard.StandardAnalyzer;
import org.apache.lucene.index.DirectoryReader;
import org.apache.lucene.index.IndexReader;
import org.apache.lucene.index.Term;
import org.apache.lucene.queryparser.classic.QueryParser;
import org.apache.lucene.queryparser.flexible.standard.QueryParserUtil;
import org.apache.lucene.search.IndexSearcher;
import org.apache.lucene.search.Query;
import org.apache.lucene.search.ScoreDoc;
import org.apache.lucene.search.TopDocs;
import org.apache.lucene.search.similarities.BM25Similarity;
import org.apache.lucene.search.similarities.ClassicSimilarity;
import org.apache.lucene.search.similarities.LMJelinekMercerSimilarity;
import org.apache.lucene.store.FSDirectory;

public class GenerateRecommendation {
	private static final double RECOMMEND_RATING = 3.5;
	
	public static void main(String[] args) {
		// TODO Auto-generated method stub
		String userIndexPath = "/Users/absheth/course/search/final_project/indexes/train_user_review/";
		String reviewsIndexPath = "/Users/absheth/course/search/final_project/indexes/rest_review/";
		String businessIndexPath = "/Users/absheth/course/search/final_project/indexes/business_index/";
		String testIndexPath = "/Users/absheth/course/search/final_project/indexes/test_index/";
		String userId = null;  // "-5e4VTnu_pR4Gpv3VSncaw" 
		 
		Map<String, Integer> hitMissMap = new HashMap<String, Integer>();
		try {
			IndexReader userIndexReader = DirectoryReader.open(FSDirectory.open(Paths.get(userIndexPath)));
			String queryString = null;
			double mape = 0.0;
			double sum = 0.0;
			for (int i = 0; i < userIndexReader.maxDoc() ; i++) {
				int noOfHits = 0;
				int actual = 0;
				userId = userIndexReader.document(i).get("user_id");
				queryString = userIndexReader.document(i).get("user_query");

				// Cold start problem
				if (queryString.length() == 0) {
					queryString = "Pizza burger fries drinks vodka whiskey";
				}
				
				IndexReader reviewIndexReader = DirectoryReader.open(FSDirectory.open(Paths.get(reviewsIndexPath)));
				IndexSearcher reviewDocSearcher = new IndexSearcher(reviewIndexReader);
				Analyzer analyzer = new StandardAnalyzer();
				QueryParser parser = new QueryParser("business_reviews", analyzer);
				
				Query query = parser.parse(QueryParserUtil.escape(queryString));
				Set<Term> queryTerms = new LinkedHashSet<Term>();
				reviewDocSearcher.createNormalizedWeight(query, false).extractTerms(queryTerms);
				reviewDocSearcher.setSimilarity(new ClassicSimilarity());
				
				// Retrieve top 500 similar documents. 
				TopDocs topDocs = reviewDocSearcher.search(query, 500);
				ScoreDoc[] scoreDocs = topDocs.scoreDocs;
				
				List recommendRestList = new ArrayList();
				int count = 0;
				for (int k = 0; k < scoreDocs.length; k++) {
					String businessId = reviewDocSearcher.doc(scoreDocs[k].doc).get("business_id");
					double docScore = scoreDocs[k].score;
					
					// Number of recommendations 
					if (recommendRestList.size() == 25) {
						break;
					}
					
					if (reviewDocSearcher.doc(scoreDocs[k].doc).get("user_list").indexOf(userId) == -1) {
						// Recommend only if the average rating for the particular business is above RECOMMENDED RATING.
						if (Double.parseDouble(reviewDocSearcher.doc(scoreDocs[k].doc).get("avg_rating")) >  RECOMMEND_RATING) {
							
							recommendRestList.add(businessId);
							
							
							// Below commented code is used to print names of the business 25 restaurants being recommended to the user.
							// It has been committed as this class gives MAPE evaluation
							/* StringBuilder top25Builder = new StringBuilder();
							
							
							BufferedWriter top25Buffer = null;
							FileWriter top25Writer = null;
							
							File top25File = new File("/Users/absheth/course/search/final_project/results/top.txt");
							
							if (!top25File.exists()) {
								top25File.createNewFile();
							}

							top25Writer = new FileWriter(top25File.getAbsoluteFile(), true);
							top25Buffer = new BufferedWriter(top25Writer);
							
							top25Builder.append(userId);
							top25Builder.append("\t");
							top25Builder.append(0);
							top25Builder.append("\t");
							top25Builder.append(businessId);
							top25Builder.append("\t");
							top25Builder.append(k+1);
							top25Builder.append("\t");
							top25Builder.append(docScore);
							top25Builder.append("\t");
							top25Builder.append("IndexTester");
							top25Builder.append("\n");
							top25Writer.write(top25Builder.toString());
							
							try {

								if (top25Buffer != null)
									top25Buffer.close();
								if (top25Writer != null)
									top25Writer.close();
								
							} catch (IOException ex) {

								ex.printStackTrace();

							}
							count++;*/
							
							
							/* IndexReader businessReader = DirectoryReader.open(FSDirectory.open(Paths.get(businessIndexPath)));
							
							for (int j = 0; j < businessReader.maxDoc(); j++) {
								// System.out.println(businessId);
								// System.out.println(businessReader.document(j).get("business_id"));
								
								if (businessReader.document(j).get("business_id").equals(businessId)) {
									if (businessReader.document(j).get("is_open").equals("1")) {
										recommendRestList.add(businessReader.document(j).get("name"));
										break;
									}
								}
							}
							businessReader.close();
							*/
							
							
						 }
					}
				}
				
				// Print businesses.
				// for (Object business : recommendRestList) {
				// 	System.out.println("Business --> " + business);
				// }
				
				// MAPE CALCULATION - FOR EVALUATION
				IndexReader testDataReader = DirectoryReader.open(FSDirectory.open(Paths.get(testIndexPath)));
				for (int j = 0; j < testDataReader.maxDoc(); j++) {
					

					if (testDataReader.document(j).get("user_id").equals(userId)) {
						String [] businesIds = testDataReader.document(j).get("businessIdList").replace("[", " ").replace("]", " ").trim().split(",");
						actual = businesIds.length;
						String businessIdList = String.join("", businesIds);
						// System.out.println(businessIdList);
						for (int l = 0; l < recommendRestList.size() ; l++) {
							if (businessIdList.indexOf((String)recommendRestList.get(l)) != -1) {
								noOfHits++;
							}
						}
						hitMissMap.put(userId, noOfHits);
						System.out.println("User ID --> " + userId + "  ||  Hits --> " + noOfHits + "  ||  Actual --> " + actual );
						sum += (actual - noOfHits) / actual;
						break;
					}
				}
				
				
				
			}
			IndexReader testDataReader = DirectoryReader.open(FSDirectory.open(Paths.get(testIndexPath)));
			System.out.println();
			// System.out.println("SUM --> " + sum);
			// System.out.println("testDataReader.maxDoc()--> " + testDataReader.maxDoc());
			mape = sum / testDataReader.maxDoc() * 100;
			System.out.println("MAPE --> " + String.format( "%.2f", mape ) + "%");
			
		} catch (Exception e) {
			e.printStackTrace();
		} finally {
			System.out.println();
			System.out.println("Completed.");
		}
	}
}
