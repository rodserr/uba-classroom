

shinyServer(function(input, output) {
    
    automobile_filtered <- reactive({
        automobile %>% 
            filter(
                make %in% input$picker_maker,
                horsepower >= input$slider_horsepower[1],
                horsepower <= input$slider_horsepower[2],     
                curb_weight >= input$slider_curb_weight[1],
                curb_weight <= input$slider_curb_weight[2],
                engine_type %in% input$check_engine_type,
                drive_wheels %in% input$check_drive_wheels
            )
    })
    
    # Cards----
    output$card_total_units <- renderText({
        
        outp <- nrow(automobile_filtered())
        
        HTML(sprintf("<div> Total Units: <br> <strong> %s </strong> </div>", outp))
        
    })
    
    output$card_mean_price <- renderText({
        
        outp <- mean(automobile_filtered()$price)
        
        HTML(sprintf("<div> Mean Price: <br> <strong> $ %s </strong> </div>", 
                     format(round(outp, 1), big.mark   = ",")))
        
    })    
    
    output$card_mae <- renderText({
        
        outp <- automobile_filtered() %>% 
            mutate(mae = abs(price-prediction)) %>% 
            pull(mae) %>% 
            mean()
        
        HTML(sprintf("<div> Mean Absolute Error: <br> <strong> $ %s </strong> </div>", 
                     format(round(outp, 1), big.mark   = ",") ))
        
    })
    
    output$card_rmse <- renderText({
        
        outp <- automobile_filtered() %>% 
            mutate(rsmae = (price-prediction)**2) %>% 
            pull(rsmae) %>% 
            mean() %>% 
            sqrt()
        
        HTML(sprintf("<div> Residual Mean Square Error: <br> <strong> $ %s </strong> </div>", 
                     format(round(outp, 1), big.mark   = ",")))
        
    })
    
    output$card_prediction <- renderText({
        
        # req(input$get_prediction)
        
        outp <- features_prediction()
        
        HTML(sprintf("<div> Price Prediction: <br> <strong> $ %s </strong> </div>", 
                     format(round(outp, 1), big.mark   = ",")))
        
    })
    
    # Plots----
    output$portfolio_composition_plot <- renderPlot({
        automobile_filtered() %>%
            count(maker_target, make) %>% 
            ggplot(aes(area = n, fill = maker_target, label = paste(make, n, sep = "\n"))) +
            treemapify::geom_treemap() +
            treemapify::geom_treemap_text(colour = "white") +
            labs(fill = 'Tier')
    })
    
    output$price_tier_plot <- renderPlot({
        automobile_filtered() %>% 
            ggplot(aes(x = maker_target, y = price, color = maker_target)) +
            geom_boxplot(alpha = .6) +
            geom_jitter(alpha = .6) +
            scale_y_continuous(labels = scales::number_format(scale = 1/1000, suffix = 'k', prefix = '$')) + 
            labs(y = 'Price', x = 'Tier', color = 'Tier')
        
    })
    
    output$price_prediction_plot <- renderPlot({
        automobile_filtered() %>% 
            ggplot(aes(x = prediction, y = price, color = maker_target)) +
            geom_point() +
            scale_y_continuous(labels = scales::number_format(scale = 1/1000, suffix = 'k', prefix = '$')) +
            scale_x_continuous(labels = scales::number_format(scale = 1/1000, suffix = 'k', prefix = '$')) +
            labs(color = 'Tier', x = 'Prediction', y = 'Price')
        
    })
    
    output$error_tier_plot <- renderPlot({
        automobile_filtered() %>% 
            ggplot(aes(y = reorder(maker_target, error), x = error, color = maker_target)) +
            geom_boxplot(alpha = .6) +
            geom_jitter(alpha = .6) +
            coord_flip() +
            scale_x_continuous(labels = scales::number_format(scale = 1/1000, suffix = 'k', prefix = '$')) +
            labs(x = 'Error (Price - Prediction)', y = 'Maker', color = 'Tier')
        
    })
    
    output$vip <- renderPlot({
        
        vip::vip(RF$fit$fit$fit)
        
    })
    
    output$error_curb_plot <- renderPlot({
        
        automobile_filtered() %>% 
            ggplot(aes(x = curb_weight, y = error)) +
            geom_point(alpha = .6) +
            geom_smooth(method = 'loess', formula = 'y~x') +
            scale_y_continuous(labels = scales::number_format(scale = 1/1000, suffix = 'k', prefix = '$')) +
            labs(y = 'Error (Price - Prediction)', x = 'Curb Weight')
        
    })
    
    output$prediction_plot <- renderPlot({
        
        .pred <- features_prediction()
        
        current_make <- isolate(input$make)
        no_current_make <- sprintf('No %s', current_make)
        
        automobile %>% 
            mutate(make_prediction = if_else(make == current_make, current_make, no_current_make)) %>% 
            ggplot(aes(x = prediction, y = 0)) +
            geom_boxplot(alpha = .3) +
            geom_jitter(aes(alpha = make_prediction), size = 2.5) +
            geom_point(data = tibble(prediction = .pred), color = '#e9832d', size = 4) +
            scale_alpha_manual(values = c(.8, .2), breaks = c(current_make, no_current_make)) +
            scale_x_continuous(labels = scales::number_format(scale = 1/1000, suffix = 'k', prefix = '$')) +
            labs(alpha = '', y = '', x = 'Price Prediction', title = 'How does this prediction compare against Portfolio?') +
            theme(axis.text.y = element_blank(), axis.ticks.y = element_blank())
    })
    
    # Prediction Inputs----
    output$prediction_features <- renderUI({
        features_map <- map2(predictors_df, names(predictors_df), make_ui)
        
        list(
            features_map[1:6] %>% make_layout(),
            features_map[7:12] %>% make_layout(),
            features_map[13:18] %>% make_layout(),
            fluidRow(
                tags$style(type='text/css', "#randomize_predictors {margin-top: 25px;}"),
                tags$style(type='text/css', "#get_prediction {margin-top: 25px;}"),
                features_map[19:21],
                column(
                    offset = 1,
                    2,
                    actionButton(
                        inputId = 'randomize_predictors',
                        label = 'Randomize Predictors',
                        icon = icon('random')
                    )
                ),
                column(
                    2,
                    actionButton(
                        inputId = 'get_prediction',
                        label = 'Get Prediction',
                        icon = icon('running'),
                        class = "btn-success"
                    )
                )
            )
        )
    })
    
    features_prediction <- eventReactive(input$get_prediction, {
        
        cat('Compute prediction \n')
        
        feature_names <- names(predictors_df)
        feature_values <- feature_names %>% map(~input[[.x]])
        names(feature_values) <- feature_names
        
        numeric_feat_scaled <- feature_values %>%
            keep(is.numeric) %>%
            flatten_df() %>%
            pivot_longer(everything(), names_to = 'variable') %>% 
            left_join(features_stats, by = 'variable') %>% 
            transmute(variable, value = (value-mean)/sd) %>% 
            pivot_wider(names_from = variable, values_from = value)
        
        .maker_target <- make_mapper %>% filter(make == input$make) %>% pull(maker_target)
        
        factor_feat <- feature_values %>%
            keep(is.character) %>%
            flatten_df() %>% 
            mutate(maker_target = .maker_target) %>% 
            select(-make)
        
        predicter_df <- bind_cols(factor_feat, numeric_feat_scaled)
        
        predict(RF, predicter_df) %>% pull() %>% exp()
        
    })
    
    observeEvent(input$randomize_predictors, {
        
        cat('Update predictors \n')
        
        map2(predictors_df, names(predictors_df), update_ui_input)
        
    })
    
})
