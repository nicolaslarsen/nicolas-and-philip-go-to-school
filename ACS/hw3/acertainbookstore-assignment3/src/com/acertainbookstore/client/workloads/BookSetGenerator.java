package com.acertainbookstore.client.workloads;

import java.util.*;

import com.acertainbookstore.business.ImmutableStockBook;
import com.acertainbookstore.business.StockBook;

/**
 * Helper class to generate stockbooks and isbns modelled similar to Random
 * class
 */
public class BookSetGenerator {

	public BookSetGenerator() {
		// TODO Auto-generated constructor stub
	}

	/**
	 * Returns num randomly selected isbns from the input set
	 * 
	 * @param num
	 * @return
	 */
	public Set<Integer> sampleFromSetOfISBNs(Set<Integer> isbns, int num) {
	    ArrayList<Integer> newIsbns = new ArrayList<>(isbns);
	    Collections.shuffle(newIsbns);
	    return new HashSet<Integer>(newIsbns.subList(0,num));
	}

	/**
	 * Return num stock books. For now return an ImmutableStockBook
	 * 
	 * @param num
	 * @return
	 */
	public Set<StockBook> nextSetOfStockBooks(int num) {
		Random rand = new Random();

		int isbn, numCopies;
		String title, author;
		float price;
		long numSaleMisses, numTimesRated, totalRating;
		boolean editorPick;

		StockBook book;
		HashSet<StockBook> nextSet = new HashSet<>();

		for (int i = 0; i < num; i++) {
			isbn = rand.nextInt(10000);
			title = "title";
			author = "author";
			price = (float) (Math.round(rand.nextFloat() * 100.0) / 100.0);
			numCopies = rand.nextInt(5000);
			numSaleMisses = rand.nextLong();
			numTimesRated = rand.nextLong();
			totalRating = numTimesRated;
			editorPick = rand.nextBoolean();

			book = new ImmutableStockBook(isbn, title, author, price, numCopies,
					numSaleMisses, numTimesRated, totalRating, editorPick);
			nextSet.add(book);
		}

		return nextSet;
	}
}
