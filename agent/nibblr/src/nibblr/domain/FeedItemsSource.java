package nibblr.domain;

import java.util.List;

public interface FeedItemsSource {

	List<FeedItem> downloadItems();
}
