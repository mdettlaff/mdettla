package mdettla.reddit.service;

import java.util.ArrayList;
import java.util.List;

import mdettla.reddit.domain.Submission;

import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import com.sun.syndication.feed.rss.Channel;
import com.sun.syndication.feed.rss.Item;

@Service
public class RssService {

	private final SubmissionService submissionService;

	@Autowired
	public RssService(SubmissionService submissionService) {
		this.submissionService = submissionService;
	}

	public Channel createRssChannel(String link) {
		Channel channel = new Channel("rss_2.0");
		channel.setTitle("Reddit");
		channel.setLink(link);
		channel.setDescription("Reddit - voice of the Internet");
		List<Item> items = new ArrayList<Item>();
		for (Submission submission : submissionService.findAll()) {
			Item item = new Item();
			item.setLink(link + "submissions/" + submission.getId());
			item.setTitle(submission.getTitle());
			items.add(item);
		}
		channel.setItems(items);
		return channel;
	}
}
